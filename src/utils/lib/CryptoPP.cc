#include <cryptopp/rsa.h>
#include <cryptopp/osrng.h>
#include <cryptopp/base64.h>
#include <cryptopp/cryptlib.h>

// Standard stuff from other gpl clients, external for mldonkey. 

typedef unsigned int wxUint32;

#define wxUINT32_SWAP_ALWAYS(val) \
   ((wxUint32) ( \
    (((wxUint32) (val) & (wxUint32) 0x000000ffU) << 24) | \
    (((wxUint32) (val) & (wxUint32) 0x0000ff00U) <<  8) | \
    (((wxUint32) (val) & (wxUint32) 0x00ff0000U) >>  8) | \
    (((wxUint32) (val) & (wxUint32) 0xff000000U) >> 24)))

#ifndef IS_LITTLE_ENDIAN
	#define wxUINT32_SWAP_ON_BE(val)  wxUINT32_SWAP_ALWAYS(val)
#else
	#define wxUINT32_SWAP_ON_BE(val)  (val)
#endif

#define ENDIAN_SWAP_32(x) (wxUINT32_SWAP_ON_BE(x))

extern "C" { 

typedef CryptoPP::RSASSA_PKCS1v15_SHA_Signer   Signer;
typedef CryptoPP::RSASSA_PKCS1v15_SHA_Verifier Verifier;


inline void PokeUInt8(void* p, uint8_t nVal)
{
  *((uint8_t*)p) = nVal;
}

inline void RawPokeUInt32(void* p, uint32_t nVal) 
{
  memcpy( p, &nVal, sizeof(uint32_t) );
}


inline void PokeUInt32(void* p, uint32_t nVal)
{
  RawPokeUInt32( p, ENDIAN_SWAP_32( nVal ) );
}


#define MAXPUBKEYSIZE 80
#define PRIVKEYSIZE 384

static Signer* s_signer = NULL;   
static CryptoPP::byte m_publicKey[MAXPUBKEYSIZE+1];
static unsigned long m_publicKeyLen = 0;

void cc_lprintf_nl(const char * msg, bool verb);

void crypto_exit () {
	if (s_signer) {
		delete (Signer*) s_signer;
	}
}

void createKey(char buf[]) {
	using namespace CryptoPP;

	try {

		std::string myString;

		AutoSeededRandomPool rng;
  	InvertibleRSAFunction privKey;
  	privKey.Initialize(rng, PRIVKEYSIZE); 

  	Base64Encoder privKeySink(new StringSink(myString), false);
  	privKey.DEREncode(privKeySink);
  	privKeySink.MessageEnd();
		std::copy(myString.begin(), myString.end(), buf);
		buf[myString.size()] = 0;

	} catch(const CryptoPP::Exception& e) {
		char buf[256]="[CryptoPP] createKey: ";
		strcat(buf, e.what());
		cc_lprintf_nl(buf, false);
	}

}

unsigned long loadKey(char privateKeyBase64[], char buf[]) {
  using namespace CryptoPP;

	unsigned long result = 0;

	try {
  
  	StringSource src(privateKeyBase64, true, new Base64Decoder);

		if (s_signer)
			delete (Signer*) s_signer;

		s_signer = new Signer(src);
		Verifier verifier(*((Signer*)s_signer));

  	ArraySink aSink(m_publicKey, 80);
  	verifier.GetMaterial().Save(aSink);
		m_publicKeyLen = aSink.TotalPutLength();
  	aSink.MessageEnd();

		memcpy(buf, m_publicKey, m_publicKeyLen);
		buf[m_publicKeyLen] = 0;

		result = m_publicKeyLen;

	} catch(const CryptoPP::Exception& e) {
		char buf[256]="[CryptoPP] loadKey: ";
		strcat(buf, e.what());
		cc_lprintf_nl(buf, false);
	}

	return result;
	
}


// return signatureSize (buf)
int createSignature(CryptoPP::byte *buf, int maxLen, CryptoPP::byte *key, int keyLen, uint32_t cInt, uint8_t ipType, uint32_t ip) {

	int result = 0;


	if (s_signer == NULL) {
		cc_lprintf_nl("createSignature: No signer", false);
		return result;
	}

	try {

		CryptoPP::SecByteBlock sBB(s_signer->SignatureLength());
		CryptoPP::AutoSeededRandomPool rng;
	
		CryptoPP::byte bArray[MAXPUBKEYSIZE+9];

		memcpy(bArray,key,keyLen);
		PokeUInt32(bArray+keyLen,cInt);   

		int extra = 0;
		if (ipType != 0) {
			extra = 5;
			PokeUInt32(bArray+keyLen+4,ip);
			PokeUInt8(bArray+keyLen+4+4,ipType);
		}

		s_signer->SignMessage(rng, bArray, keyLen+4+extra, sBB.begin());
		CryptoPP::ArraySink aSink(buf, maxLen);
		aSink.Put(sBB.begin(), sBB.size());
		result =  aSink.TotalPutLength();

	} catch(const CryptoPP::Exception& e) {
		char buf[256]="[CryptoPP] createSignature: ";
		strcat(buf, e.what());
		cc_lprintf_nl(buf, false);
	}

	return result;

}

int verifySignature(CryptoPP::byte *key, int keyLen, CryptoPP::byte *sig, int sigLen, uint32_t cInt, uint8_t ipType, uint32_t ip) {
  using namespace CryptoPP;

	bool result = false;

	try {

		StringSource ss_Pubkey(key, keyLen,true,0);
		Verifier pubKey(ss_Pubkey);

		CryptoPP::byte bArray[MAXPUBKEYSIZE+9];
	
		memcpy(bArray,m_publicKey,m_publicKeyLen);
		PokeUInt32(bArray+m_publicKeyLen,cInt); 

		int extra = 0;
		if (ipType != 0) {
			extra = 5;
			PokeUInt32(bArray+m_publicKeyLen+4,ip);
			PokeUInt8(bArray+m_publicKeyLen+4+4,ipType);
		}
		result = pubKey.VerifyMessage(bArray, m_publicKeyLen+4+extra, sig, sigLen);

	} catch(const CryptoPP::Exception& e) {
		char buf[256]="[CryptoPP] verifySignature: ";
		strcat(buf, e.what());
		cc_lprintf_nl(buf, true);
	}

	return result ? 1 : 0;
}


}

