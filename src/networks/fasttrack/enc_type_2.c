/*
 * Copyright (C) 2003 Markus Kern (mkern@users.sourceforge.net)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

/*
 * This file was relayed to me and is originally from Raimar Falke.
 * I cleaned it up a bit to save bandwidth.
 * Used for encryption version 0x02
 */

typedef unsigned char u8;
typedef unsigned int u32;

static u32 mix_major0(u32 *state, u32 extra_state);

void enc_type_2 (unsigned char *key, unsigned int seed)
{
	mix_major0 ((unsigned int *)key, seed);
}

static u32 mix_major0(u32 *state, u32 extra_state);
static u32 mix_major1(u32 *state, u32 extra_state);
static u32 mix_major2(u32 *state, u32 extra_state);
static u32 mix_major3(u32 *state, u32 extra_state);
static u32 mix_major4(u32 *state, u32 extra_state);
static u32 mix_major5(u32 *state, u32 extra_state);
static u32 mix_major6(u32 *state, u32 extra_state);
static u32 mix_major7(u32 *state, u32 extra_state);
static u32 mix_major8(u32 *state, u32 extra_state);
static u32 mix_major9(u32 *state, u32 extra_state);
static u32 mix_major10(u32 *state, u32 extra_state);
static u32 mix_major11(u32 *state, u32 extra_state);
static u32 mix_major12(u32 *state, u32 extra_state);
static u32 mix_major13(u32 *state, u32 extra_state);
static u32 mix_major14(u32 *state, u32 extra_state);
static u32 mix_major15(u32 *state, u32 extra_state);
static u32 mix_major16(u32 *state, u32 extra_state);
static u32 mix_major17(u32 *state, u32 extra_state);
static u32 mix_major18(u32 *state, u32 extra_state);
static u32 mix_major19(u32 *state, u32 extra_state);
static u32 mix_major20(u32 *state, u32 extra_state);
static u32 mix_major21(u32 *state, u32 extra_state);
static u32 mix_major22(u32 *state, u32 extra_state);
static u32 mix_major23(u32 *state, u32 extra_state);
static u32 mix_major24(u32 *state, u32 extra_state);
static u32 mix_minor0(u32 *state, u32 extra_state);
static u32 mix_minor1(u32 *state, u32 extra_state);
static u32 mix_minor2(u32 *state, u32 extra_state);
static u32 mix_minor3(u32 *state, u32 extra_state);
static u32 mix_minor4(u32 *state, u32 extra_state);
static u32 mix_minor5(u32 *state, u32 extra_state);
static u32 mix_minor6(u32 *state, u32 extra_state);
static u32 mix_minor7(u32 *state, u32 extra_state);
static u32 mix_minor8(u32 *state, u32 extra_state);
static u32 mix_minor9(u32 *state, u32 extra_state);
static u32 mix_minor10(u32 *state, u32 extra_state);
static u32 mix_minor11(u32 *state, u32 extra_state);
static u32 mix_minor12(u32 *state, u32 extra_state);
static u32 mix_minor13(u32 *state, u32 extra_state);
static u32 mix_minor14(u32 *state, u32 extra_state);
static u32 mix_minor15(u32 *state, u32 extra_state);
static u32 mix_minor16(u32 *state, u32 extra_state);
static u32 mix_minor17(u32 *state, u32 extra_state);
static u32 mix_minor18(u32 *state, u32 extra_state);
static u32 mix_minor19(u32 *state, u32 extra_state);
static u32 mix_minor20(u32 *state, u32 extra_state);
static u32 mix_minor21(u32 *state, u32 extra_state);
static u32 mix_minor22(u32 *state, u32 extra_state);
static u32 mix_minor23(u32 *state, u32 extra_state);
static u32 mix_minor24(u32 *state, u32 extra_state);
static u32 mix_minor25(u32 *state, u32 extra_state);
static u32 mix_minor26(u32 *state, u32 extra_state);
static u32 mix_minor27(u32 *state, u32 extra_state);
static u32 mix_minor28(u32 *state, u32 extra_state);
static u32 mix_minor29(u32 *state, u32 extra_state);
static u32 mix_minor30(u32 *state, u32 extra_state);
static u32 mix_minor31(u32 *state, u32 extra_state);
static u32 mix_minor32(u32 *state, u32 extra_state);
static u32 mix_minor33(u32 *state, u32 extra_state);
static u32 mix_minor34(u32 *state, u32 extra_state);
static u32 mix_minor35(u32 *state, u32 extra_state);
static u32 mix_minor36(u32 *state, u32 extra_state);
static u32 mix_minor37(u32 *state, u32 extra_state);
static u32 mix_minor38(u32 *state, u32 extra_state);
static u32 mix_minor39(u32 *state, u32 extra_state);
static u32 mix_minor40(u32 *state, u32 extra_state);
static u32 mix_minor41(u32 *state, u32 extra_state);
static u32 mix_minor42(u32 *state, u32 extra_state);
static u32 mix_minor43(u32 *state, u32 extra_state);
static u32 mix_minor44(u32 *state, u32 extra_state);
static u32 mix_minor45(u32 *state, u32 extra_state);
static u32 mix_minor46(u32 *state, u32 extra_state);
static u32 mix_minor47(u32 *state, u32 extra_state);
static u32 mix_minor48(u32 *state, u32 extra_state);
static u32 mix_minor49(u32 *state, u32 extra_state);
static u32 mix_minor50(u32 *state, u32 extra_state);
static u32 mix_minor51(u32 *state, u32 extra_state);
static u32 mix_minor52(u32 *state, u32 extra_state);
static u32 mix_minor53(u32 *state, u32 extra_state);
static u32 mix_minor54(u32 *state, u32 extra_state);
static u32 mix_minor55(u32 *state, u32 extra_state);
static u32 mix_minor56(u32 *state, u32 extra_state);
static u32 mix_minor57(u32 *state, u32 extra_state);
static u32 mix_minor58(u32 *state, u32 extra_state);
static u32 mix_minor59(u32 *state, u32 extra_state);
static u32 mix_minor60(u32 *state, u32 extra_state);
static u32 mix_minor61(u32 *state, u32 extra_state);
static u32 mix_minor62(u32 *state, u32 extra_state);
static u32 mix_minor63(u32 *state, u32 extra_state);
static u32 mix_minor64(u32 *state, u32 extra_state);
static u32 mix_minor65(u32 *state, u32 extra_state);
static u32 mix_minor66(u32 *state, u32 extra_state);
static u32 mix_minor67(u32 *state, u32 extra_state);
static u32 mix_minor68(u32 *state, u32 extra_state);
static u32 mix_minor69(u32 *state, u32 extra_state);
static u32 call(u32 addr, u32 *state, u32 extra_state);

static u32 ROR(u32 value, u32 count)
{
 count = (count & 0xff) % 32;
 return (value >> count) | (value << (32 - count));
}

static u32 ROL(u32 value, u32 count)
{
 count = (count & 0xff) % 32;
 return (value << count) | (value >> (32 - count));
}


u32 mix_major0(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 edi = extra_state;           
 ebx = edi;               
 ebx = ebx ^ state[19];       
 ebx = ebx ^ state[15];       
 ebx = ebx % 0x0B;
 local_u32_0 = ebx;           
 edx = state[6] * 0x1bb4a70d;    
 edx = edx * state[8];       
 state[6] = edx;            
 ecx = state[12];            
 ecx = ecx + 0xe087bd96;      
 ecx = ecx + state[14];       
 state[12] = ecx;            
 eax = state[13];            
 eax = eax | 0x39367989;       
 edi = edi & eax;          

 if (7 == ebx) {            
  ebx = 0x5f50cd07;          
  eax = 0xa8b5f6f9 + ebx;     
  eax = call(eax, state, 0xec9ed2c1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xcd17376;      
  eax = call(ebx, state, 0x828f1883); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x7fedfe28;     
  eax = call(ebx, state, 0xd02b60f3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd8e0ef9;      
  eax = call(ebx, state, state[19]);  
 }

 eax = edi;               
 ecx = 0x1f;              
 eax = ROR(eax, ecx);          
 state[14] = state[14] - eax;    
 ebx = edi;               
 ebx = ebx & 0x8e30c76;       
 state[8] = state[8] ^ ebx;     
 edx = state[12];            
 edx = edx ^ 0xd05f635;       
 edx = edx * state[3];       
 state[3] = edx;            
 ecx = state[10];            
 ecx = ecx + ecx;          
 ecx = ecx + 0xa92dc43a;      
 state[10] = ecx;            

 if (0 == local_u32_0) {        
  ebx = 0x2acaab48;          
  eax = 0xdd3b3250 + ebx;     
  eax = call(eax, state, 0x91fc47a9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x673914f0;     
  eax = call(ebx, state, 0x2590bef0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x84d24a2;      
  eax = call(ebx, state, 0x2c669b26); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13b5d4bf;     
  eax = call(ebx, state, state[5]);  
 }

 eax = state[17];            
 edi = 0xff92b824 +  eax + edi ; 
 edx = state[3];            
 eax = edx;               
 eax = eax ^ 0x62c448c0;      
 state[1] = state[1] + eax;     
 eax = state[8];            
 eax = eax ^ 0x43c25efd;      
 eax = eax ^ edx;          
 state[8] = eax;            
 edx = state[9];            
 eax = edx;               
 ecx = 0x10;              
 eax = ROL(eax, ecx);          
 edx = edx ^ eax;          
 state[9] = edx;            

 if (5 == local_u32_0) {        
  ebx = 0x1ead6fcc;          
  eax = 0xe95c732c + ebx;     
  eax = call(eax, state, 0x863938ef); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x385884a8;     
  eax = call(ebx, state, 0x58da27f4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2817dc29;     
  eax = call(ebx, state, 0x51e80a45); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5970efc8;     
  eax = call(ebx, state, state[5]);  
 }

 eax = state[3];            
 eax = eax + 0xef553b21;      
 eax = eax - state[2];       
 state[3] = eax;            
 eax = state[18];            
 eax = eax + 0x3b26991e;      
 eax = eax + state[13];       
 state[18] = eax;            

 if (4 == local_u32_0) {        
  ebx = 0x1190b5fd;          
  eax = 0xf6772843 + ebx;     
  eax = call(eax, state, 0xbfa21ddc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x477c46e2;     
  eax = call(ebx, state, 0x4fedb0f5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf3777b1;      
  eax = call(ebx, state, 0xf6783a2a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x128873ac;     
  eax = call(ebx, state, state[7]);  
 }

 edx = state[11];            
 edx = edx & 0x4be050d;       
 edx = edx + state[12];       
 state[12] = edx;            
 eax = edi;               
 ecx = 0x8;               
 eax = ROR(eax, ecx);          
 state[17] = state[17] ^ eax;    
 ebx = state[16];            
 ebx = ebx + 0xf7;         
 eax = state[8];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[8] = eax;            
 eax = eax + 0xf3910fa;       
 edx = edx * eax;          
 state[12] = edx;            

 if (2 == local_u32_0) {        
  ebx = 0x28dd9703;          
  eax = 0xdf284059 + ebx;     
  eax = call(eax, state, 0xcbbfcc48); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13c43c50;     
  eax = call(ebx, state, 0xc3a7a161); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c9b8cf7;     
  eax = call(ebx, state, 0x9cffc26b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4b5da352;     
  eax = call(ebx, state, edi);     
 }

 eax = state[8];            
 eax = eax + 0x4088eb5f;      
 eax = eax + edi;          
 state[8] = eax;            
 eax = state[7];            
 eax = eax ^ 0x1387a250;      
 state[5] = state[5] & eax;     
 eax = state[1];            
 eax = eax ^ 0x47f3a78b;      
 state[2] = state[2] | eax;     
 eax = state[10] * 0x1d208465;   
 state[17] = state[17] | eax;    

 if (1 == local_u32_0) {        
  ebx = 0x231b1479;          
  eax = 0xe4ecc9c7 + ebx;     
  eax = call(eax, state, 0x48ca60c);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x19bff38e;     
  eax = call(ebx, state, 0x28fc4e79); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x279ea40d;     
  eax = call(ebx, state, 0xa7bd5f5a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6433f0b6;     
  eax = call(ebx, state, state[9]);  
 }

 eax = edi;               
 eax = eax & 0x4be5deac;      
 state[1] = state[1] - eax;     
 eax = state[15];            
 eax = eax & 0x3496b61a;      
 state[4] = state[4] + eax;     

 if (10 == local_u32_0) {        
  ebx = 0x1721eecb;          
  eax = 0xf0e7fa49 + ebx;     
  eax = call(eax, state, 0x3ad6754a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xee2d3f9;      
  eax = call(ebx, state, 0x3be35f99); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9278fd4;      
  eax = call(ebx, state, 0xcaceada2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29f42662;     
  eax = call(ebx, state, state[8]);  
 }

 ebx = edi * 0xffffffb0;      
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 edx = state[12];            
 edx = edx + 0x6b465da;       
 state[13] = state[13] & edx;    

 if (3 == local_u32_0) {        
  ebx = 0x25708edd;          
  eax = 0xe2960637 + ebx;     
  eax = call(eax, state, 0x6fd435f0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d3fa489;     
  eax = call(ebx, state, 0xc1903471); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x149c284e;     
  eax = call(ebx, state, 0x7846089f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4e937a56;     
  eax = call(ebx, state, state[11]);  
 }

 eax = state[14];            
 eax = eax + 0xff7068bf;      
 state[16] = state[16] | eax;    
 eax = state[19];            
 eax = eax ^ 0x1e569f2b;      
 state[7] = state[7] & eax;     
 eax = state[15] * 0x49f90b6a;   
 state[12] = state[12] + eax;    

 if (6 == local_u32_0) {        
  ebx = 0xaea651c;           
  eax = 0xfd1c5aec + ebx;     
  eax = call(eax, state, 0xd5456f37); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x7a984c7b;     
  eax = call(ebx, state, 0x8ef487de); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x23022cdc;     
  eax = call(ebx, state, 0xe92764c1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x171a0123;     
  eax = call(ebx, state, state[7]);  
 }

 eax = state[18] * 0xb0223a7;    
 state[6] = state[6] - eax;     
 edx = state[17];            
 edx = edx + 0x19da7ccb;      
 edx = edx + state[3];       
 state[17] = edx;            
 eax = state[4] * 0x4f4bc59;    
 state[19] = state[19] - eax;    
 eax = edi;               
 eax = eax & 0x3a423827;      
 edx = edx - eax;          
 state[17] = edx;            

 if (9 == local_u32_0) {        
  ebx = 0x28c25d8b;          
  eax = 0xdf461419 + ebx;     
  eax = call(eax, state, 0xc4e7f9c9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4898a548;     
  eax = call(ebx, state, 0x18122df);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a1780e4;     
  eax = call(ebx, state, 0x245e6d52); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb9a137b;      
  eax = call(ebx, state, edi);     
 }

 eax = state[11];            
 edi = 0xea268d79 +  eax + edi ; 
 eax = eax + 0x7b41453;       
 edi = edi ^ eax;          

 if (8 == local_u32_0) {        
  ebx = 0xf7da8de;           
  eax = 0xf88ac932 + ebx;     
  eax = call(eax, state, 0xfe003ce6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3c2eada9;     
  eax = call(ebx, state, 0x78d525ec); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x208b92a8;     
  eax = call(ebx, state, 0xf91e3f43); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x833a0d6;      
  eax = call(ebx, state, edi);     
 }

 ecx = state[0];            
 ecx = ecx ^ 0x361eddb9;      
 eax = state[2];            
 local_u32_3 = eax;           
 ecx = ecx ^ eax;          
 local_u32_1 = ecx;           
 state[0] = ecx;            
 ebx = state[4];            
 ebx = ebx + 0xf9;         
 eax = state[4];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 ebx = eax;               
 state[4] = ebx;            
 edx = local_u32_1;           
 edx = 0xc3201c46 +  edi + edx ; 
 state[0] = edx;            
 ecx = state[16];            
 ecx = ecx + 0xf6c0ea7;       
 ecx = ecx * state[8];       
 local_u32_2 = ecx;           
 state[8] = ecx;            
 eax = state[18] * 0x11e13f53;   
 local_u32_1 = eax;           
 eax = state[11];            
 ecx = local_u32_1;           
 eax = ROR(eax, ecx);          
 state[11] = eax;            
 local_u32_3 = local_u32_3 | 0x5747f7c; 
 ebx = ebx | local_u32_3;      
 state[2] = ebx;            
 edx = state[3] * 0x336a3c4f;    
 edi = edi ^ edx;          
 ecx = 0x2b702a62 + edi;      
 eax = state[9];            
 ecx = ecx ^ eax;          
 edx = local_u32_2;           
 edx = edx + 0x5ff3732;       
 ecx = ecx ^ edx;          
 state[9] = ecx;            
 ecx = state[1];            
 ecx = ecx + 0xfa4e2f52;      
 ecx = ecx * state[1];       
 state[1] = ecx;            
 eax = edi;               
 return eax;
}


u32 mix_major1(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 eax = state[12];            
 eax = eax ^ state[9];       
 edx = state[6];            
 eax = eax ^ edx;          
 local_u32_2 = eax;           
 esi = local_u32_2 % 0x0B;
 eax = extra_state;           
 eax = eax + 0x2183b643;      
 edx = state[3];            
 local_u32_1 = edx;           
 local_u32_1 = local_u32_1 & 0x24398ab; 
 ecx = local_u32_1;           
 state[5] = state[5] + ecx;     
 eax = 0x24631391 +  edx + eax ; 
 edx = state[18];            
 eax = eax - edx;          
 local_u32_0 = eax;           

 if (2 == esi) {            
  ebx = 0x6a6d9974;          
  eax = 0x9d98fb80 + ebx;     
  eax = call(eax, state, 0x69f1e613); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1c9b43e7;     
  eax = call(ebx, state, 0xb2461575); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29431d6b;     
  eax = call(ebx, state, 0xbc73e0ff); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x512274f2;     
  eax = call(ebx, state, state[19]);  
 }

 eax = state[14];            
 eax = eax + 0xc0fd80ba;      
 local_u32_0 = local_u32_0 ^ eax;  
 eax = state[1] * 0xe99b672;    
 state[12] = state[12] - eax;    
 eax = state[0];            
 eax = eax + 0xca70bf60;      
 state[15] = state[15] ^ eax;    

 if (1 == esi) {            
  ebx = 0x210eb2e7;          
  eax = 0xe6f80d45 + ebx;     
  eax = call(eax, state, 0x13db4bec); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10e81e47;     
  eax = call(ebx, state, 0x17885b09); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2d15d4a5;     
  eax = call(ebx, state, 0x214006c6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a444cb4;     
  eax = call(ebx, state, state[11]);  
 }

 eax = local_u32_0;           
 eax = eax ^ 0x17339c6;       
 local_u32_0 = local_u32_0 + eax;  
 eax = state[7] * 0x15f0a011;    
 eax = eax + state[15];       
 state[15] = eax;            
 edx = state[17];            
 ecx = 0x1b597286 + edx;      
 state[4] = state[4] & ecx;     
 eax = eax & 0x389e630b;      
 edx = edx * eax;          
 state[17] = edx;            

 if (3 == esi) {            
  ebx = 0xa24e013;           
  eax = 0xfde3da95 + ebx;     
  eax = call(eax, state, 0xe47fc3c0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a0c63e0;     
  eax = call(ebx, state, 0x14a32681); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf535100;      
  eax = call(ebx, state, 0x5d77fb22); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x69c49b7b;     
  eax = call(ebx, state, state[3]);  
 }

 ecx = state[18];            
 ecx = ecx ^ 0x31a138ce;      
 ecx = ecx ^ state[19];       
 state[18] = ecx;            
 eax = local_u32_0 * 0x271fe1f1;  
 state[16] = state[16] & eax;    
 ebx = state[16];            
 ebx = ebx ^ 0x29;         
 eax = state[7];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[7] = eax;            

 if (5 == esi) {            
  ebx = 0x4aa39479;          
  ebx = ebx + 0xbd652653;     
  eax = call(ebx, state, 0x8cf196c0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5aef696f;     
  eax = call(ebx, state, 0x8271071);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8282716;      
  eax = call(ebx, state, 0x489ed59f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x25e4ff52;     
  edx = local_u32_0;          
  eax = call(ebx, state, edx);     
 }

 eax = state[6];            
 ecx = 0x1f;              
 eax = ROR(eax, ecx);          
 eax = eax * state[2];       
 state[2] = eax;            
 edx = local_u32_0;           
 edx = edx + 0xfee822a8;      
 ecx = state[14];            
 edx = edx - ecx;          
 eax = edx * 0x9dfbe4;       
 eax = eax * state[5];       
 edx = state[13];            
 edx = 0xfd2ead2f +  edx + eax ; 
 local_u32_0 = edx;           

 if (9 == esi) {            
  ebx = 0x110670c6;          
  eax = 0xf7024ae2 + ebx;     
  eax = call(eax, state, 0x5d331920); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9314b65;      
  eax = call(ebx, state, 0xb702090e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x23905e6d;     
  eax = call(ebx, state, 0x28394e8a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb2da803;      
  eax = call(ebx, state, state[18]);  
 }

 ecx = state[7];            
 ecx = ecx + 0x2b29baf9;      
 ecx = ecx + state[17];       
 state[7] = ecx;            
 ebx = state[0];            
 ecx = 0x19;              
 ebx = ROL(ebx, ecx);          
 eax = state[2];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[2] = eax;            

 if (6 == esi) {            
  ebx = 0x1e721dc3;          
  eax = 0xe9969d55 + ebx;     
  eax = call(eax, state, 0x12314128); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3aa83d76;     
  eax = call(ebx, state, 0xca477dca); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x392786b1;     
  eax = call(ebx, state, 0x23ef45d7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f4a0bf0;     
  eax = call(ebx, state, state[4]);  
 }

 eax = state[5] * 0x282f40d5;    
 local_u32_0 = local_u32_0 - eax;  
 eax = state[16];            
 eax = eax + 0x2223fa4b;      
 state[12] = state[12] & eax;    
 eax = state[18];            
 ecx = 0x10;              
 eax = ROR(eax, ecx);          
 local_u32_0 = local_u32_0 & eax;  

 if (8 == esi) {            
  ebx = 0x1fda2667;          
  ebx = ebx + 0xe82e94b1;     
  eax = call(ebx, state, 0x60ffb509); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4d5b9499;     
  eax = call(ebx, state, 0xcf7247dd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1afdd132;     
  eax = call(ebx, state, 0x48b70310); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14e94bd9;     
  eax = local_u32_0;          
  eax = call(ebx, state, eax);     
 }

 edx = state[17];            
 edx = edx + 0xf9ac8515;      
 eax = state[7];            
 edx = edx + eax;          
 state[17] = edx;            
 eax = eax + state[10];       
 eax = eax + 0xf9b69577;      
 state[7] = eax;            

 if (4 == esi) {            
  ebx = 0x15312d82;          
  eax = 0xf2d6233e + ebx;     
  eax = call(eax, state, 0x375ebf07); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x198daaac;     
  eax = call(ebx, state, 0x7c04333);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb4aa056;      
  eax = call(ebx, state, 0x4c285c11); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x289ffaaf;     
  eax = call(ebx, state, state[13]);  
 }

 eax = state[13];            
 eax = eax ^ 0x6d56f7f;       
 state[7] = state[7] + eax;     
 eax = state[8];            
 eax = eax + 0x8c8d3d9c;      
 eax = eax - local_u32_0;      
 state[8] = eax;            

 if (7 == esi) {            
  ebx = 0x1b5a58d0;          
  eax = 0xecab9574 + ebx;     
  eax = call(eax, state, 0x404a4067); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd8d9db1;      
  eax = call(ebx, state, 0x2230c395); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2cf21292;     
  eax = call(ebx, state, 0x58e89828); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa200ca2;      
  eax = call(ebx, state, state[4]);  
 }

 eax = state[5];            
 eax = eax + 0x4d2bd380;      
 eax = eax - state[12];       
 state[5] = eax;            
 ecx = state[1];            
 ecx = ecx + 0xfcee8aad;      
 ecx = ecx - local_u32_0;      
 state[1] = ecx;            
 eax = state[18];            
 edx = eax + 8 * eax;          
 edx = edx << 2;            
 edx = edx - eax;          
 edx = eax + 8 * edx;          
 edx = eax + 8 * edx;          
 eax = 4 * edx;             
 eax = eax - edx;          
 eax = eax << 6;            
 eax = eax * ecx;          
 state[18] = eax;            
 eax = state[4];            
 eax = eax + 0xdc2745dc;      
 eax = eax * state[8];       
 state[8] = eax;            

 if (10 == esi) {            
  ebx = 0x3b62ad60;          
  eax = 0xcca329fc + ebx;     
  eax = call(eax, state, 0xc6cb6995); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3cadd51f;     
  eax = call(ebx, state, 0x2929b86);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d63f237;     
  eax = call(ebx, state, 0x261970a0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x276b6cfc;     
  eax = call(ebx, state, state[2]);  
 }

 edx = state[7];            
 edx = edx + 0xd9de0ed7;      
 ecx = state[18];            
 edx = edx ^ ecx;          
 local_u32_2 = edx;           
 state[18] = edx;            
 ebx = state[6];            
 ebx = ebx + 0x71;         
 eax = state[11];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[11] = eax;            
 edx = state[19];            
 edx = edx + 0xb295dc;       
 edx = edx + local_u32_2;      
 state[19] = edx;            

 if (0 == esi) {            
  ebx = 0x98d24d1;           
  ebx = ebx + 0xfe7b4d1b;     
  eax = call(ebx, state, 0x4faf7da4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x430baa55;     
  eax = call(ebx, state, 0xf086a434); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4b5c18e6;     
  eax = call(ebx, state, 0x24526842); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa1ffa8b;      
  ecx = local_u32_0;          
  eax = call(ebx, state, ecx);     
 }

 ebx = local_u32_0;           
 ebx = ebx | 0x58eafd;        
 eax = state[15];            
 eax = eax - ebx;          
 local_u32_3 = eax;           
 state[15] = eax;            
 edx = state[12];            
 ebx = edx * 0xffffffc6;      
 eax = state[5];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[5] = eax;            
 esi = state[2];            
 esi = esi + 0xf42fd441;      
 esi = esi + state[19];       
 state[2] = esi;            
 local_u32_1 = esi;           
 local_u32_1 = local_u32_1 | 0x10d913b8; 
 edx = edx * local_u32_1;      
 state[12] = edx;            
 ebx = state[11];            
 edx = 0x2039d1f9 + ebx;      
 ecx = state[1];            
 edx = edx ^ ecx;          
 local_u32_2 = edx;           
 state[1] = edx;            
 eax = esi;               
 ecx = 0x3;               
 eax = ROR(eax, ecx);          
 edx = local_u32_3;           
 edx = edx + eax;          
 state[15] = edx;            
 ecx = local_u32_2;           
 ebx = 0x55f96491 +  ecx + ebx ; 
 state[11] = ebx;            
 edx = edx & 0x864fe18;       
 edx = edx * state[4];       
 state[4] = edx;            
 eax = local_u32_0;           
 edx = state[10];            
 eax = 0x2ff6c3cc +  edx + eax ; 
 local_u32_0 = eax;           
 edx = state[18];            
 edx = edx + 0xf5eb4571;      
 edx = edx * state[18];       
 state[18] = edx;            
 return eax;
}


u32 mix_major2(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_3;
 edi = extra_state;           
 edx = state[9] % 0x0B;
 local_u32_0 = edx;           
 ecx = state[0];            
 ecx = ecx | 0x4d9f89df;       
 ecx = ecx | edi;          
 state[0] = ecx;            
 ebx = edi;               
 ebx = ebx & 0x10691818;      
 edi = edi - ebx;          
 eax = state[15];            
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 edx = state[15];            
 edx = edx & eax;          
 state[15] = edx;            

 if (2 == local_u32_0) {        
  ebx = 0x57894d98;          
  eax = 0xb07d7600 + ebx;     
  eax = call(eax, state, 0xfbf48d65); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8e608c2;      
  eax = call(ebx, state, 0x70c92c02); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x284089ee;     
  eax = call(ebx, state, 0xb02e6efb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2db761e7;     
  eax = call(ebx, state, state[9]);  
 }

 ecx = state[7];            
 ecx = ecx ^ 0x1f11181f;      
 edi = edi | ecx;          
 ebx = state[18];            
 ebx = ebx + 0x18;         
 eax = state[17];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[17] = eax;            
 edx = state[18];            
 edx = edx + 0xc18379a4;      
 state[3] = state[3] & edx;     
 ecx = state[8];            
 ecx = ecx + 0x8845990;       
 ecx = ecx + state[2];       
 state[8] = ecx;            

 if (8 == local_u32_0) {        
  ebx = 0x26a4a163;          
  eax = 0xe1621e4d + ebx;     
  eax = call(eax, state, 0x25323763); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x286ce3f0;     
  eax = call(ebx, state, 0x21afc536); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xed778f7;      
  eax = call(ebx, state, 0x1e4c2347); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xaa419d0;      
  eax = call(ebx, state, edi);     
 }

 eax = state[7];            
 ecx = 0x1e;              
 eax = ROL(eax, ecx);          
 edi = edi * eax;          
 ebx = edi;               
 ebx = ebx ^ 0x61;         
 eax = state[14];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[14] = eax;            
 edx = state[3];            
 edx = edx ^ 0x1a11c1c;       
 state[3] = state[3] - edx;     

 if (3 == local_u32_0) {        
  ebx = 0xb6b8a99;           
  eax = 0xfc9e5e9f + ebx;     
  eax = call(eax, state, 0x7536608b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30dbd554;     
  eax = call(ebx, state, 0x9b43e646); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f84397f;     
  eax = call(ebx, state, 0x52535671); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f77893c;     
  eax = call(ebx, state, edi);     
 }

 eax = state[5];            
 eax = eax + 0xbdf50793;      
 eax = eax + edi;          
 state[5] = eax;            
 eax = state[6];            
 eax = eax ^ 0x341c6ce5;      
 edi = edi - eax;          
 eax = state[14];            
 eax = eax | 0x11712ba;       
 edi = edi ^ eax;          
 eax = state[4];            
 eax = eax + 0x1df0f08c;      
 eax = eax - edi;          
 state[4] = eax;            

 if (4 == local_u32_0) {        
  ebx = 0x9d10923;           
  eax = 0xfe37b185 + ebx;     
  eax = call(eax, state, 0xf86aaa73); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe35c113;      
  eax = call(ebx, state, 0x1a29e230); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1886cd1d;     
  eax = call(ebx, state, 0x465d1441); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c223af4;     
  eax = call(ebx, state, state[2]);  
 }

 eax = state[15];            
 eax = eax + 0xd8a810b1;      
 edi = edi * eax;          
 eax = state[0];            
 eax = eax + 0x8e4e3c5;       
 eax = eax - state[7];       
 state[0] = eax;            
 eax = edi;               
 eax = eax ^ 0x13f1a8da;      
 state[9] = state[9] - eax;     

 if (7 == local_u32_0) {        
  ebx = 0x1f0b3d67;          
  eax = 0xe8fa0e61 + ebx;     
  eax = call(eax, state, 0x7b6ddac5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x134e16ff;     
  eax = call(ebx, state, 0x2425a128); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x99745ec;      
  eax = call(ebx, state, 0x72049ca2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e1f3e77;     
  eax = call(ebx, state, state[12]);  
 }

 eax = 0xf2dd8a98 + edi;      
 eax = eax ^ state[14];       
 state[14] = eax;            
 edx = state[3];            
 edx = edx & 0xb51383c;       
 eax = eax | edx;          
 state[14] = eax;            

 if (1 == local_u32_0) {        
  ebx = 0x442617bb;          
  eax = 0xc3e1c019 + ebx;     
  eax = call(eax, state, 0x88f8d35d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x77bb3057;     
  eax = call(ebx, state, 0x7fd3d182); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1cdea49d;     
  eax = call(ebx, state, 0x18222407); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5b845f7c;     
  eax = call(ebx, state, state[9]);  
 }

 eax = state[0];            
 eax = eax + 0x16bda446;      
 edx = state[2];            
 eax = eax - edx;          
 state[0] = eax;            
 eax = eax ^ 0x3576dfb9;      
 edx = edx - eax;          
 state[2] = edx;            

 if (9 == local_u32_0) {        
  ebx = 0x1285ca82;          
  eax = 0xf580ca72 + ebx;     
  eax = call(eax, state, 0xf858a0fc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2bf1386d;     
  eax = call(ebx, state, 0xfbe68a32); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2f7c6448;     
  eax = call(ebx, state, 0x7069477);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa07aacb;      
  eax = call(ebx, state, state[9]);  
 }

 eax = state[6];            
 eax = eax | 0x1720cf3;       
 state[5] = state[5] - eax;     
 eax = state[16];            
 eax = eax ^ 0x2dfed60;       
 eax = eax ^ state[19];       
 state[16] = eax;            
 eax = state[12];            
 eax = eax + 0xffcf5d22;      
 edi = edi * eax;          
 eax = state[11];            
 eax = eax ^ 0x26b4296;       
 edi = edi + eax;          

 if (6 == local_u32_0) {        
  ebx = 0x11a65de8;          
  eax = 0xf6638b2c + ebx;     
  eax = call(eax, state, 0x4de0fdb2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5df4e4f1;     
  eax = call(ebx, state, 0xd70512e7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x36e4bdd6;     
  eax = call(ebx, state, 0x46d51d34); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16c52d15;     
  eax = call(ebx, state, edi);     
 }

 ebx = edi;               
 ecx = 0xb;               
 ebx = ROL(ebx, ecx);          
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 eax = state[17];            
 ecx = 0x19;              
 eax = ROL(eax, ecx);          
 edi = edi - eax;          
 edx = state[3];            
 edx = edx ^ 0x125c14db;      
 state[4] = state[4] + edx;     

 if (0 == local_u32_0) {        
  ebx = 0x37a5e0f5;          
  eax = 0xd0626bbf + ebx;     
  eax = call(eax, state, 0x33e6ff92); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13d331a3;     
  eax = call(ebx, state, 0x4e055014); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb2b1cbb;      
  eax = call(ebx, state, 0x224ebd80); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x20ae8949;     
  eax = call(ebx, state, state[9]);  
 }

 eax = state[15];            
 ecx = 0x1f;              
 eax = ROR(eax, ecx);          
 eax = eax | state[9];        
 state[9] = eax;            
 edi = edi + 0xfde54451;      
 edi = edi - state[19];       
 edx = state[11];            
 edx = edx | 0x3;          
 ecx = edx;               
 eax = ROR(eax, ecx);          
 state[9] = eax;            
 eax = state[18];            
 eax = eax ^ 0x22da8ee3;      
 eax = eax ^ edi;          
 state[18] = eax;            

 if (10 == local_u32_0) {        
  ebx = 0x89bba58;           
  eax = 0xff6d00c0 + ebx;     
  eax = call(eax, state, 0x292c53f8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13adb3e6;     
  eax = call(ebx, state, 0xcbffd850); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4f64bf4e;     
  eax = call(ebx, state, 0x1df7b162); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xdf3d432;      
  eax = call(ebx, state, state[2]);  
 }

 ebx = state[9];            
 ebx = ebx + 0x8f;         
 eax = state[2];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[2] = eax;            
 ebx = state[18];            
 ebx = ebx & 0x53;         
 eax = state[5];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[5] = eax;            
 edx = state[1];            
 edx = edx ^ 0x2822999;       
 eax = eax - edx;          
 state[5] = eax;            

 if (5 == local_u32_0) {        
  ebx = 0x18b614ab;          
  eax = 0xef52a621 + ebx;     
  eax = call(eax, state, 0x4cf0e3f7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5ac26dfb;     
  eax = call(ebx, state, 0x54c9ac57); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9774b32;      
  eax = call(ebx, state, 0x79c6b50b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x47036796;     
  eax = call(ebx, state, state[9]);  
 }

 eax = edi;               
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 edx = state[1];            
 eax = eax + edx;          
 local_u32_3 = eax;           
 state[1] = eax;            
 ebx = state[4];            
 local_u32_1 = ebx;           
 local_u32_1 = local_u32_1 | 0x161d3ea; 
 ecx = local_u32_1;           
 state[6] = state[6] + ecx;     
 eax = state[9];            
 eax = eax + 0xc2e590c;       
 eax = eax + edi;          
 state[9] = eax;            
 local_u32_1 = edi;           
 local_u32_1 = local_u32_1 ^ 0x125deacd; 
 edi = edi - local_u32_1;      
 edx = state[17];            
 edx = edx ^ 0x10b015bf;      
 state[7] = state[7] & edx;     
 state[17] = 0x1bb396c0;        
 local_u32_3 = local_u32_3 & 0x7a04e3e; 
 eax = eax * local_u32_3;      
 state[9] = eax;            
 ecx = state[16];            
 ecx = ecx | 0x16cf1fa2;       
 edi = edi + ecx;          
 edi = edi * 0x1d5ac40e;      
 edi = edi * state[14];       
 eax = 0xf27819a7 + edi;      
 ebx = ebx ^ eax;          
 state[4] = ebx;            
 eax = edi;               
 return eax;
}


u32 mix_major3(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0;
 edi = extra_state;           
 ebx = edi;               
 ebx = ebx ^ state[16];       
 ebx = ebx ^ state[10];       
 ebx = ebx % 0x0B;
 local_u32_0 = ebx;           
 edx = state[3];            
 edx = edx & 0x19997dc0;      
 edx = edx * state[12];       
 state[12] = edx;            
 ecx = state[0];            
 ecx = ecx + 0xd31e211;       
 edi = edi | ecx;          
 eax = state[14];            
 eax = eax + 0x7cfa160;       
 eax = eax - state[0];       
 state[14] = eax;            

 if (10 == ebx) {            
  ebx = 0x2b7fce85;          
  eax = 0xdc8a1ab3 + ebx;     
  eax = call(eax, state, 0x4a3d1e20); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa296f5f;      
  eax = call(ebx, state, 0x29830e2b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1458a637;     
  eax = call(ebx, state, 0x8df0232e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3ddee5bd;     
  eax = call(ebx, state, state[2]);  
 }

 ebx = edi;               
 ebx = ebx + 0xe6;         
 eax = state[13];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[13] = eax;            
 edx = state[12];            
 edx = edx + 0xfd1d773c;      
 edx = edx * state[3];       
 state[3] = edx;            

 if (3 == local_u32_0) {        
  ebx = 0x5e386114;          
  eax = 0xa9cf76e4 + ebx;     
  eax = call(eax, state, 0xab4316e);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x96e92db;      
  eax = call(ebx, state, 0x35811c20); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2601d425;     
  eax = call(ebx, state, 0x268ea793); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x935b8b7;      
  eax = call(ebx, state, state[12]);  
 }

 eax = 0xd10c7a44 + edi;      
 edi = edi | eax;          
 eax = state[0];            
 edi = 0xf3754e81 +  eax + edi ; 
 edi = edi ^ 0x21d2a427;      
 ebx = state[16];            
 edi = edi ^ ebx;          

 if (1 == local_u32_0) {        
  ebx = 0x240b9374;          
  eax = 0xe3fc44a8 + ebx;     
  eax = call(eax, state, 0xa0726217); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2162f986;     
  eax = call(ebx, state, 0xebe9adea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x18f33cd2;     
  eax = call(ebx, state, 0x4aef76e9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2873cc3a;     
  eax = call(ebx, state, state[11]);  
  ebx = state[16];           
 }

 ebx = ebx | 0x599c0b2;       
 ebx = ebx | state[4];        
 state[16] = ebx;            
 ebx = state[0];            
 ebx = ebx + 0xdd;         
 eax = edi;               
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 edi = eax;               
 edx = state[6];            
 edx = edx ^ 0x1d86d59a;      
 state[3] = state[3] & edx;     
 ecx = state[0];            
 ecx = ecx ^ 0x22d79e78;      
 ecx = ecx ^ state[10];       
 state[0] = ecx;            

 if (9 == local_u32_0) {        
  ebx = 0xb7a5ec6;           
  eax = 0xfc8e5c2e + ebx;     
  eax = call(eax, state, 0xf1ca6995); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x41b239e5;     
  eax = call(ebx, state, 0xc7a7026f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x145fc996;     
  eax = call(ebx, state, 0x5ac62a91); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6028c940;     
  eax = call(ebx, state, state[16]);  
 }

 ebx = state[9];            
 ebx = ebx + 0x22;         
 eax = state[15];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[15] = eax;            
 eax = edi;               
 ecx = 0xd;               
 eax = ROL(eax, ecx);          
 edi = edi + eax;          
 edx = state[4];            
 edx = edx ^ 0x17568f8b;      
 edi = edi + edx;          
 ecx = state[9];            
 ecx = ecx ^ 0x1b7d211b;      
 state[3] = state[3] - ecx;     

 if (7 == local_u32_0) {        
  ebx = 0x5e1345dc;          
  eax = 0xa9f43984 + ebx;     
  eax = call(eax, state, 0xce485b83); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5af64f89;     
  eax = call(ebx, state, 0x79bfdc2b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1bda8dc2;     
  eax = call(ebx, state, 0x34447906); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a2cef27;     
  eax = call(ebx, state, edi);     
 }

 eax = state[10];            
 eax = eax ^ 0x25da4024;      
 eax = eax * state[14];       
 state[14] = eax;            
 eax = state[19];            
 eax = eax ^ 0x195596e2;      
 state[3] = state[3] + eax;     

 if (8 == local_u32_0) {        
  ebx = 0x8bae576;           
  eax = 0xff4cf282 + ebx;     
  eax = call(eax, state, 0xff14be4);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xfae15d5;      
  eax = call(ebx, state, 0x7449c112); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x32962bdf;     
  eax = call(ebx, state, 0x48d82db3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x31bea080;     
  eax = call(ebx, state, edi);     
 }

 eax = state[4];            
 ecx = 0xb;               
 eax = ROR(eax, ecx);          
 state[3] = state[3] ^ eax;     
 eax = state[2];            
 eax = eax & 0x142c74fa;      
 state[19] = state[19] ^ eax;    
 state[7] = 0x3de4cf2b;         
 eax = state[5] * 0x1195dbf3;    
 edi = edi ^ eax;          

 if (5 == local_u32_0) {        
  ebx = 0x1627134e;          
  eax = 0xf1ded7a2 + ebx;     
  eax = call(eax, state, 0xd8727bd4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5077df4c;     
  eax = call(ebx, state, 0x7740dede); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x36a92381;     
  eax = call(ebx, state, 0x261f9532); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29c34520;     
  eax = call(ebx, state, state[9]);  
 }

 eax = state[12] * 0x25bf72d4;   
 eax = eax * state[14];       
 state[12] = eax;            
 eax = state[11];            
 ecx = 0x2;               
 eax = ROL(eax, ecx);          
 edi = edi + eax;          

 if (0 == local_u32_0) {        
  ebx = 0x35948686;          
  eax = 0xd2720eb2 + ebx;     
  eax = call(eax, state, 0x53d65d3);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x26980233;     
  eax = call(ebx, state, 0x8f8f65ec); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x202e3938;     
  eax = call(ebx, state, 0x25394681); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d743d1e;     
  eax = call(ebx, state, edi);     
 }

 eax = state[7];            
 eax = eax + 0xfbd89057;      
 eax = eax + edi;          
 state[7] = eax;            
 eax = state[12];            
 eax = eax + 0xfec898a3;      
 eax = eax - edi;          
 state[12] = eax;            
 eax = 0xe6d9d0ce + edi;      
 edi = edi * eax;          
 eax = state[2] * 0x25d5927e;    
 eax = eax * state[0];       
 state[2] = eax;            

 if (6 == local_u32_0) {        
  ebx = 0x265ee12b;          
  eax = 0xe1a990c1 + ebx;     
  eax = call(eax, state, 0x4cd340f8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x46206a89;     
  eax = call(ebx, state, 0xd7c29b2);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b8ac6b0;     
  eax = call(ebx, state, 0x3df99882); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb34c4cb;      
  eax = call(ebx, state, state[3]);  
 }

 eax = edi;               
 eax = eax ^ 0x7951f14a;      
 edi = edi - eax;          
 eax = state[19];            
 eax = eax ^ 0x159fa550;      
 edi = edi * eax;          
 eax = edi * 0x1b0d12a6;      
 state[9] = state[9] - eax;     

 if (4 == local_u32_0) {        
  ebx = 0x3a76aafe;          
  eax = 0xcd8f2c5e + ebx;     
  eax = call(eax, state, 0xe903bb6d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e7eaaf3;     
  eax = call(ebx, state, 0x7883c4fc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xbf47c0e;      
  eax = call(ebx, state, 0x361be44f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe7f6c37;      
  eax = call(ebx, state, edi);     
 }

 edx = state[4];            
 edx = edx | 0xf2ff1db;       
 edx = edx + state[12];       
 state[12] = edx;            
 eax = edi * 0x1a41598b;      
 state[7] = state[7] - eax;     
 eax = state[14];            
 eax = eax & 0x36ff2c0;       
 eax = eax * state[17];       
 state[17] = eax;            
 eax = state[2];            
 eax = eax & 0xac8676c;       
 edx = edx ^ eax;          
 state[12] = edx;            

 if (2 == local_u32_0) {        
  ebx = 0x13f1950d;          
  eax = 0xf417269b + ebx;     
  eax = call(eax, state, 0x6d5e127e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1984534f;     
  eax = call(ebx, state, 0x4cb99db);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8fdfc2e;      
  eax = call(ebx, state, 0xa9d77057); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x50c3714b;     
  eax = call(ebx, state, state[11]);  
 }

 eax = state[11];            
 eax = eax + 0xe311654d;      
 state[19] = state[19] ^ eax;    
 edx = state[16];            
 eax = edx * 0x1267cd78;      
 state[18] = state[18] ^ eax;    
 ecx = edi;               
 ecx = ecx ^ 0x1c8b2015;      
 ecx = ecx & edx;          
 eax = ecx;               
 eax = eax | 0xc26f29a;       
 ecx = ecx * eax;          
 state[16] = ecx;            
 eax = state[10];            
 eax = eax + 0xcec46d19;      
 state[14] = state[14] & eax;    
 eax = state[0];            
 eax = eax + 0x32ff9d2f;      
 edi = edi * eax;          
 eax = state[8];            
 eax = eax ^ 0xc03874d;       
 state[8] = state[8] - eax;     
 eax = state[13];            
 edi = 0xf9b39aca +  eax + edi ; 
 eax = edi;               
 return eax;
}


u32 mix_major4(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2;
 edi = extra_state;           
 ebx = state[15];            
 local_u32_1 = ebx;           
 eax = state[2];            
 local_u32_1 = local_u32_1 ^ eax;  
 local_u32_1 = local_u32_1 ^ edi;  
 ecx = local_u32_1 % 0x09;
 local_u32_0 = ecx;           
 eax = state[14];            
 eax = eax + 0xd3892fe6;      
 eax = eax + ebx;          
 state[14] = eax;            
 ecx = state[2];            
 ecx = ecx + 0xe600fde6;      
 ecx = ecx - edi;          
 state[2] = ecx;            
 eax = state[4];            
 eax = eax + 0x385e38e;       
 ebx = ebx ^ eax;          
 state[15] = ebx;            
 ecx = 0xc6189f52 + edi;      
 state[18] = state[18] | ecx;    

 if (7 == local_u32_0) {        
  ebx = 0x34ac018e;          
  eax = 0xd35c7112 + ebx;     
  eax = call(eax, state, 0x8252e111); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3be0f0a9;     
  eax = call(ebx, state, 0x15d9d426); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x37416b90;     
  eax = call(ebx, state, 0x2b312900); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1501fab9;     
  eax = call(ebx, state, state[1]);  
 }

 eax = state[14];            
 eax = eax + 0x8f6f81a9;      
 ecx = state[11];            
 eax = eax & ecx;          
 local_u32_1 = eax;           
 state[11] = eax;            
 edx = state[6];            
 ecx = 0xe;               
 edx = ROR(edx, ecx);          
 eax = state[12];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[12] = eax;            
 edx = state[8];            
 ecx = 0xe;               
 edx = ROL(edx, ecx);          
 edi = edi - edx;          
 edx = local_u32_1;           
 ecx = 0x1;               
 edx = ROR(edx, ecx);          
 eax = state[0];            
 ecx = edx;               
 eax = ROR(eax, ecx);          
 edx = eax;               
 local_u32_1 = local_u32_1 ^ 0x43cd4d14; 
 edx = edx + local_u32_1;      
 state[0] = edx;            

 if (3 == local_u32_0) {        
  ebx = 0x4ebbd73a;          
  eax = 0xb94a063a + ebx;     
  eax = call(eax, state, 0x9c42ffbc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa58c56b;      
  eax = call(ebx, state, 0xc8aebfe8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x26b10d25;     
  eax = call(ebx, state, 0x6469b080); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x19b0b7e7;     
  eax = call(ebx, state, state[14]);  
 }

 eax = state[8];            
 eax = eax ^ 0x155c464;       
 state[3] = state[3] - eax;     
 eax = state[16];            
 eax = eax + 0xf8d647b6;      
 eax = eax + state[0];       
 state[16] = eax;            
 eax = state[2];            
 eax = eax ^ 0x11e3788d;      
 eax = eax ^ state[4];       
 state[2] = eax;            

 if (5 == local_u32_0) {        
  ebx = 0x13cf516f;          
  eax = 0xf4396a11 + ebx;     
  eax = call(eax, state, 0x1045494f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d63af96;     
  eax = call(ebx, state, 0xedce12aa); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9e93ec5;      
  eax = call(ebx, state, 0x1c3772b0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2d444d6f;     
  eax = call(ebx, state, state[10]);  
 }

 eax = edi;               
 ecx = 0xb;               
 eax = ROL(eax, ecx);          
 edi = edi | eax;          
 eax = edi;               
 eax = eax ^ 0x16984b90;      
 state[5] = state[5] & eax;     
 eax = edi;               
 ecx = 0x1d;              
 eax = ROR(eax, ecx);          
 state[16] = state[16] + eax;    
 eax = state[0];            
 eax = eax + 0xc3e56f16;      
 eax = eax + state[15];       
 state[0] = eax;            

 if (2 == local_u32_0) {        
  ebx = 0x21a117d6;          
  eax = 0xe667a342 + ebx;     
  eax = call(eax, state, 0x2a391a7e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12a2498c;     
  eax = call(ebx, state, 0xc9dcd626); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a353157;     
  eax = call(ebx, state, 0xbb1d0460); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6ae42057;     
  eax = call(ebx, state, state[1]);  
 }

 eax = state[11];            
 eax = eax + 0xe57356e7;      
 state[5] = state[5] & eax;     
 edx = edi;               
 edx = edx ^ 0x23f157f6;      
 ecx = state[18];            
 ecx = ecx - edx;          
 state[18] = ecx;            
 ecx = ecx & 0x155b7cc8;      
 edi = edi - ecx;          

 if (1 == local_u32_0) {        
  ebx = 0x2072eaea;          
  eax = 0xe796fe4e + ebx;     
  eax = call(eax, state, 0x12ac20fd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x37f5f6b8;     
  eax = call(ebx, state, 0x1b423418); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x22653795;     
  eax = call(ebx, state, 0xef8f976a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xc276936;      
  eax = call(ebx, state, edi);     
 }

 eax = state[8];            
 eax = eax | 0x21496d22;       
 eax = eax | state[5];        
 state[8] = eax;            
 edi = edi + 0x93b1543f;      
 edi = edi - state[18];       
 ecx = state[14] * 0x1db47609;   
 ecx = ecx * edi;          
 state[14] = ecx;            
 edx = state[10];            
 edx = edx ^ 0x5a;         
 eax = state[7];            
 ecx = edx;               
 eax = ROR(eax, ecx);          
 edx = eax;               
 state[7] = edx;            
 ecx = state[18];            
 ecx = ecx + 0x7c;         
 edx = ROL(edx, ecx);          
 state[7] = edx;            

 if (0 == local_u32_0) {        
  ebx = 0xe4d52be;           
  eax = 0xf9bb685a + ebx;     
  eax = call(eax, state, 0x2e8d72cd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf4cbf71;      
  eax = call(ebx, state, 0x45257d1a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x87981bb;      
  eax = call(ebx, state, 0xe981d59d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16bc8ac7;     
  eax = call(ebx, state, edi);     
 }

 eax = state[5];            
 ecx = 0x3;               
 eax = ROL(eax, ecx);          
 state[8] = state[8] ^ eax;     
 eax = state[6];            
 eax = eax ^ 0x2c8ca15;       
 eax = eax ^ edi;          
 state[6] = eax;            
 eax = edi;               
 ecx = 0xd;               
 eax = ROL(eax, ecx);          
 state[13] = state[13] + eax;    

 if (4 == local_u32_0) {        
  ebx = 0xa50fe1f;           
  eax = 0xfdb47649 + ebx;     
  eax = call(eax, state, 0x45975315); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13346140;     
  eax = call(ebx, state, 0x76f29932); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x59e9b8db;     
  eax = call(ebx, state, 0x444a096f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x24febf7d;     
  eax = call(ebx, state, state[18]);  
 }

 eax = state[2];            
 eax = eax & 0xa0962e5;       
 state[17] = state[17] ^ eax;    
 eax = edi;               
 eax = eax & 0xd505f52;       
 eax = eax * state[3];       
 state[3] = eax;            
 eax = state[15];            
 eax = eax ^ 0x15284f42;      
 edi = edi - eax;          

 if (8 == local_u32_0) {        
  ebx = 0x3cf97e81;          
  eax = 0xcb0e5977 + ebx;     
  eax = call(eax, state, 0xe139a689); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3ee72842;     
  eax = call(ebx, state, 0xf179d0b9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x209acd6b;     
  eax = call(ebx, state, 0x37f1d8d8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1affa6c5;     
  eax = call(ebx, state, state[8]);  
 }

 eax = state[2];            
 eax = eax + 0xf8df2963;      
 state[7] = state[7] & eax;     
 eax = state[6] * 0x256b9c9c;    
 eax = eax * edi;          
 state[6] = eax;            
 eax = state[1];            
 eax = eax | 0xda16d9b;       
 state[10] = state[10] + eax;    
 eax = state[5];            
 eax = eax ^ 0x28b62e0c;      
 eax = eax * state[9];       
 state[9] = eax;            

 if (6 == local_u32_0) {        
  ebx = 0x77a59d42;          
  eax = 0x9061e21e + ebx;     
  eax = call(eax, state, 0xdcc8f292); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28771678;     
  eax = call(ebx, state, 0xbc3315f);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x497e81d2;     
  eax = call(ebx, state, 0xf32374a9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30fdef6e;     
  eax = call(ebx, state, edi);     
 }

 eax = state[5] * 0x23779c9e;    
 ecx = state[12];            
 eax = eax ^ ecx;          
 local_u32_2 = eax;           
 state[12] = eax;            
 ebx = state[19];            
 eax = ebx;               
 ecx = 0x1d;              
 eax = ROR(eax, ecx);          
 eax = eax * state[10];       
 state[10] = eax;            
 edx = state[0];            
 edx = edx ^ 0x38a5f94;       
 edx = edx ^ eax;          
 state[0] = edx;            
 ecx = state[15];            
 edi = 0x1c82e95e +  ecx + edi ; 
 edx = state[5];            
 edx = edx ^ 0xfd;         
 eax = state[9];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 edx = eax;               
 state[9] = edx;            
 ecx = local_u32_2;           
 ecx = 0xc0e4fa7d +  edi + ecx ; 
 state[12] = ecx;            
 eax = state[17];            
 eax = eax ^ 0x141bbf98;      
 eax = eax ^ state[7];       
 state[17] = eax;            
 eax = state[18];            
 ecx = 0x6;               
 eax = ROR(eax, ecx);          
 edx = edx ^ eax;          
 state[9] = edx;            
 ecx = state[13];            
 ecx = ecx & 0x2373fe39;      
 state[4] = state[4] - ecx;     
 eax = edi;               
 ecx = 0xf;               
 eax = ROL(eax, ecx);          
 ebx = ebx + eax;          
 state[19] = ebx;            
 eax = edi;               
 return eax;
}


u32 mix_major5(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 edx = state[18] % 0x0B;
 local_u32_0 = edx;           
 ecx = state[17] * 0x2e7a089;    
 state[5] = state[5] | ecx;     
 eax = state[13];            
 eax = eax + 0x1fef7de0;      
 state[3] = state[3] ^ eax;     
 edx = state[16];            
 edx = edx ^ 0x8338b85;       
 edi = extra_state;           
 edi = edi - edx;          

 if (0 == local_u32_0) {        
  ebx = 0x2e133cec;          
  eax = 0xd9f6ac00 + ebx;     
  eax = call(eax, state, 0x27888b0f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x94bcabc;      
  eax = call(ebx, state, 0x2b13a34b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14dfce14;     
  eax = call(ebx, state, 0x737f8364); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1256e655;     
  eax = call(ebx, state, state[11]);  
 }

 edi = edi * 0x1cd19bfb;      
 eax = state[12];            
 eax = eax + 0x15bdbb56;      
 eax = eax * state[3];       
 state[3] = eax;            
 eax = 0x374580a7 + edi;      
 state[11] = state[11] ^ eax;    
 eax = edi;               
 eax = eax | 0x86941f3;       
 state[10] = state[10] + eax;    

 if (4 == local_u32_0) {        
  ebx = 0xc039377;           
  eax = 0xfc03c335 + ebx;     
  eax = call(eax, state, 0x7e0a8b27); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1485f302;     
  eax = call(ebx, state, 0x1c99372);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4dd38e67;     
  eax = call(ebx, state, 0xf21181dc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x129ee1ac;     
  eax = call(ebx, state, state[18]);  
 }

 eax = state[16];            
 eax = eax ^ 0x11119dd6;      
 state[6] = state[6] - eax;     
 eax = state[13];            
 eax = eax + 0xcb82c76c;      
 eax = eax + state[18];       
 state[13] = eax;            
 eax = state[1];            
 eax = eax ^ 0x3b98ae58;      
 state[8] = state[8] - eax;     

 if (9 == local_u32_0) {        
  ebx = 0xce60f05;           
  eax = 0xfb22637b + ebx;     
  eax = call(eax, state, 0x2e6bf52);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x845d440;      
  eax = call(ebx, state, 0x696f0e34); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4714dbf5;     
  eax = call(ebx, state, 0xed6bd0e6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16517dd7;     
  eax = call(ebx, state, state[1]);  
 }

 eax = state[17];            
 eax = eax + 0xcfd5283;       
 state[17] = state[17] ^ eax;    
 eax = state[13];            
 eax = eax + 0x539ef62;       
 state[5] = state[5] & eax;     
 eax = state[14];            
 eax = eax ^ 0x639b87fe;      
 state[11] = state[11] & eax;    

 if (8 == local_u32_0) {        
  ebx = 0x30194450;          
  eax = 0xd7ec9924 + ebx;     
  eax = call(eax, state, 0x227d0086); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb203aaa;      
  eax = call(ebx, state, 0x88914e5d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x77ae3093;     
  eax = call(ebx, state, 0x2925fd1d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa128e22;      
  eax = call(ebx, state, state[18]);  
 }

 ebx = edi;               
 ebx = ebx | 0x369e02e;       
 ecx = state[12];            
 ecx = ecx - ebx;          
 local_u32_1 = ecx;           
 state[12] = ecx;            
 ecx = ecx + 0xf0544c52;      
 ecx = ecx * state[6];       
 state[6] = ecx;            
 eax = state[5];            
 edi = 0x8dcb06 +  eax + edi ;  
 ebx = edi;               
 ebx = ebx & 0x632ffca;       
 edx = local_u32_1;           
 edx = edx - ebx;          
 state[12] = edx;            

 if (3 == local_u32_0) {        
  ebx = 0x193a2479;          
  eax = 0xeece9707 + ebx;     
  eax = call(eax, state, 0x1ab2160f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x23d779d2;     
  eax = call(ebx, state, 0x1414f80d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe644caa;      
  eax = call(ebx, state, 0x2d87d79a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1353d76b;     
  eax = call(ebx, state, state[17]);  
 }

 ecx = state[6] * 0x345114ef;    
 state[16] = state[16] - ecx;    
 ebx = state[11] * 0xf6123d0;    
 eax = state[10];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[10] = eax;            
 edx = state[4];            
 edx = edx & 0x18b74e25;      
 state[0] = state[0] + edx;     

 if (7 == local_u32_0) {        
  ebx = 0x4cde9202;          
  eax = 0xbb2a28ca + ebx;     
  eax = call(eax, state, 0x50e04b1f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x569da8c0;     
  eax = call(ebx, state, 0x515eb4cf); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x64ee7407;     
  eax = call(ebx, state, 0xb8e88545); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4f5f68e2;     
  eax = call(ebx, state, state[15]);  
 }

 ecx = state[2];            
 ecx = ecx ^ 0x18f1b56;       
 state[2] = state[2] - ecx;     
 ebx = state[13];            
 ebx = ebx + 0x66;         
 eax = state[19];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[19] = eax;            

 if (6 == local_u32_0) {        
  ebx = 0xf0764b7;           
  eax = 0xf8ffec09 + ebx;     
  eax = call(eax, state, 0x3d2cd180); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1fa0f321;     
  eax = call(ebx, state, 0xdc7b4010); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xbb9a065;      
  eax = call(ebx, state, 0x489ba0b3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x140d1ed5;     
  eax = call(ebx, state, edi);     
 }

 ebx = state[7];            
 ebx = ebx + 0x56;         
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 ebx = state[14];            
 ebx = ebx + 0xf1;         
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 edi = eax;               

 if (5 == local_u32_0) {        
  ebx = 0x1c1e5446;          
  eax = 0xebe8fc7a + ebx;     
  eax = call(eax, state, 0x2dec9dea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xedef96f;      
  eax = call(ebx, state, 0x4b828647); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6414149c;     
  eax = call(ebx, state, 0xec347331); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x140d4523;     
  eax = call(ebx, state, state[7]);  
 }

 edi = edi + 0xd26e6435;      
 eax = state[10];            
 local_u32_1 = eax;           
 edi = edi - eax;          
 ebx = state[13];            
 ebx = ebx ^ 0x95;         
 eax = state[8];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[8] = eax;            
 local_u32_1 = local_u32_1 ^ 0x1da5a5e2; 
 edx = local_u32_1;           
 state[1] = state[1] + edx;     

 if (2 == local_u32_0) {        
  ebx = 0x515b5baa;          
  eax = 0xb6ad5f4a + ebx;     
  eax = call(eax, state, 0xc82da065); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd5ae29e;      
  eax = call(ebx, state, 0xf4d6723);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1c4ce042;     
  eax = call(ebx, state, 0xbb1c556c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8f1fd8d;      
  eax = call(ebx, state, state[10]);  
 }

 eax = edi * 0x1665683f;      
 state[7] = state[7] | eax;     
 eax = state[6];            
 eax = eax + 0xd3198985;      
 eax = eax + state[17];       
 state[6] = eax;            
 eax = state[1] * 0xb2490cd;    
 edi = edi & eax;          

 if (1 == local_u32_0) {        
  ebx = 0x16d39371;          
  eax = 0xf1327a4f + ebx;     
  eax = call(eax, state, 0xb2147e70); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x529eb058;     
  eax = call(ebx, state, 0x890fd277); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29272df6;     
  eax = call(ebx, state, 0x1a1dbabb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x47cea136;     
  eax = call(ebx, state, state[6]);  
 }

 eax = state[3];            
 eax = eax ^ 0x49caa386;      
 state[13] = state[13] - eax;    
 eax = state[5];            
 eax = eax + 0xca44ad;       
 eax = eax - state[7];       
 state[5] = eax;            
 eax = state[14];            
 eax = eax | 0xce2b27d;       
 edi = edi + eax;          

 if (10 == local_u32_0) {        
  ebx = 0x464dfc38;          
  eax = 0xc1b98294 + ebx;     
  eax = call(eax, state, 0xfe10f4a5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8cfae96;      
  eax = call(ebx, state, 0x77550b75); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x160e320e;     
  eax = call(ebx, state, 0x47252b3e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e4a0fe4;     
  eax = call(ebx, state, state[11]);  
 }

 ecx = state[15];            
 ecx = ecx + 0x9f72b74b;      
 eax = state[6];            
 ecx = ecx + eax;          
 local_u32_2 = ecx;           
 state[15] = ecx;            
 edx = state[16];            
 edx = edx + 0xaa1914c0;      
 edx = edx - state[3];       
 state[16] = edx;            
 eax = edi;               
 ecx = 0x12;              
 eax = ROL(eax, ecx);          
 edi = edi - eax;          
 edx = state[14];            
 edx = edx ^ 0x7a9f2d9;       
 edx = edx ^ state[9];       
 state[14] = edx;            
 eax = state[3];            
 ecx = 0xa;               
 eax = ROL(eax, ecx);          
 state[19] = state[19] & eax;    
 edi = edi * 0xd49e9d9;       
 edi = edi * local_u32_2;      
 edx = state[2];            
 edx = edx ^ 0xc52d715;       
 ecx = state[4];            
 edx = edx + ecx;          
 local_u32_3 = edx;           
 state[4] = edx;            
 eax = local_u32_2 * 0x300c07b6;  
 eax = eax * state[11];       
 state[15] = eax;            
 edx = edi * 0x59c5268;       
 edx = edx ^ local_u32_3;      
 state[4] = edx;            
 ecx = state[7];            
 ecx = ecx + 0xf1ae26ce;      
 ecx = ecx - edi;          
 state[7] = ecx;            
 eax = edi;               
 return eax;
}


u32 mix_major6(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_2;
 edx = state[5];            
 local_u32_0 = edx;           
 ecx = state[3];            
 local_u32_0 = local_u32_0 ^ ecx;  
 edx = state[18];            
 local_u32_0 = local_u32_0 ^ edx;  
 local_u32_0 = local_u32_0 & 0x7;  
 eax = state[7];            
 eax = eax ^ 0x3610ff4;       
 eax = eax ^ state[5];       
 state[7] = eax;            
 eax = state[14];            
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 eax = eax ^ edx;          
 state[18] = eax;            
 eax = state[10];            
 eax = eax + 0x4e;         
 edx = state[15];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[15] = edx;            
 edi = state[1];            
 edi = edi + 0xa89a8207;      
 edi = edi ^ extra_state;      
 edi = edi & 0xecc2fa7d;      

 if (0 == local_u32_0) {        
  ebx = 0x32b03fc4;          
  eax = 0xd557170c + ebx;     
  eax = call(eax, state, 0x109742c7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x189b5822;     
  eax = call(ebx, state, 0x8b7d007b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16e0c53d;     
  eax = call(ebx, state, 0x688d0504); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x340f1a08;     
  eax = call(ebx, state, edi);     
 }

 ecx = state[0] * 0x19dd786;    
 edx = state[15];            
 ecx = ecx ^ edx;          
 local_u32_2 = ecx;           
 state[15] = ecx;            
 eax = edi;               
 ecx = 0xc;               
 eax = ROL(eax, ecx);          
 edi = edi * eax;          
 eax = edi;               
 eax = eax | 0x1249d1c;       
 state[17] = state[17] & eax;    
 eax = state[8];            
 eax = eax + 0x5e67551f;      
 eax = eax ^ local_u32_2;      
 state[15] = eax;            
 edx = state[0] * 0x320ea6ec;    
 edi = edi + edx;          
 eax = state[19];            
 eax = eax + 0xee10c43d;      
 edi = edi ^ eax;          

 if (1 == local_u32_0) {        
  ebx = 0x7e2a9327;          
  eax = 0x89db7a71 + ebx;     
  eax = call(eax, state, 0xf8fa5a1e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8d42255;      
  eax = call(ebx, state, 0x27ec9d71); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x92e5dca;      
  eax = call(ebx, state, 0x1ab0326e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x17feee33;     
  eax = call(ebx, state, state[2]);  
 }

 eax = edi;               
 eax = eax ^ 0x67;         
 edx = state[15];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[15] = edx;            
 eax = state[14] * 0x54cc1685;   
 ecx = state[5];            
 ecx = ecx - eax;          
 eax = ecx;               
 state[5] = eax;            
 edx = state[12];            
 edx = edx + 0xf7d8f2fa;      
 edx = edx - edi;          
 state[12] = edx;            
 eax = eax + 0xf95da87e;      
 eax = eax - state[10];       
 state[5] = eax;            
 eax = state[8];            
 ecx = 0x12;              
 eax = ROL(eax, ecx);          
 edi = edi ^ eax;          

 if (5 == local_u32_0) {        
  ebx = 0x8f46ee6;           
  eax = 0xff157412 + ebx;     
  eax = call(eax, state, 0x8e0505a3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5783ac4c;     
  eax = call(ebx, state, 0x10629f3e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd7ec711;      
  eax = call(ebx, state, 0x11d6ae05); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4d0a7114;     
  eax = call(ebx, state, state[16]);  
 }

 edx = state[2];            
 local_u32_2 = edx;           
 eax = edx;               
 eax = eax ^ 0x4983faaa;      
 state[19] = state[19] + eax;    
 edi = edi & 0x911ab6a;       
 edi = edi & state[6];       
 eax = edx;               
 eax = eax + 0xfbb4acd7;      
 state[17] = state[17] & eax;    
 eax = state[5];            
 eax = eax + 0xf96465d3;      
 eax = eax + state[13];       
 state[5] = eax;            
 eax = state[2];            
 eax = eax | 0x99;          
 edx = edi;               
 ecx = eax;               
 edx = ROL(edx, ecx);          
 edi = edx;               
 local_u32_2 = local_u32_2 | 0x176f7fa2; 
 ecx = local_u32_2;           
 state[9] = state[9] + ecx;     

 if (7 == local_u32_0) {        
  ebx = 0x4e9897d5;          
  eax = 0xb9714b23 + ebx;     
  eax = call(eax, state, 0xfd4538d5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14870cf9;     
  eax = call(ebx, state, 0xf9f86d39); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13896a3c;     
  eax = call(ebx, state, 0x832df7f9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12525cd8;     
  eax = call(ebx, state, edi);     
 }

 eax = edi;               
 eax = eax + 0xd0;         
 edx = state[4];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[4] = edx;            
 eax = state[6];            
 eax = eax ^ 0x1ae616e0;      
 edi = edi | eax;          
 eax = state[15];            
 eax = eax << 7;            
 eax = eax - state[15];       
 eax = eax << 7;            
 eax = eax + state[15];       
 ecx = state[15];            
 eax = ecx + 2 * eax;          
 eax = ecx + 4 * eax;          
 eax = eax << 2;            
 edi = edi ^ eax;          
 eax = state[2];            
 eax = eax + 0x83;         
 edx = state[14];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[14] = edx;            

 if (6 == local_u32_0) {        
  ebx = 0x3a1580d4;          
  eax = 0xcdf33a84 + ebx;     
  eax = call(eax, state, 0xb49ff199); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x26b2eb20;     
  eax = call(ebx, state, 0x4397eea6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4e5745eb;     
  eax = call(ebx, state, 0x95bdeb6f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x642a0397;     
  eax = call(ebx, state, edi);     
 }

 eax = state[10];            
 eax = eax & 0x1311b0aa;      
 state[12] = state[12] - eax;    
 eax = 0xf5736e40 + edi;      
 state[14] = state[14] ^ eax;    
 eax = state[18];            
 ecx = 0xf;               
 eax = ROR(eax, ecx);          
 state[17] = state[17] + eax;    
 eax = state[11];            
 eax = eax + 0x25e8d98c;      
 edi = edi ^ eax;          
 eax = state[14];            
 eax = eax | 0x28;          
 edx = state[0];            
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[0] = edx;            
 eax = state[3];            
 eax = eax ^ 0x2a68c40c;      
 state[13] = state[13] - eax;    

 if (3 == local_u32_0) {        
  ebx = 0x14670688;          
  eax = 0xf3a0f7e4 + ebx;     
  eax = call(eax, state, 0x23bf1d0a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f33e098;     
  eax = call(ebx, state, 0x4df565cd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf8fc628;      
  eax = call(ebx, state, 0x6162ad12); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x435987f7;     
  eax = call(ebx, state, state[3]);  
 }

 eax = edi;               
 ecx = 0xc;               
 eax = ROR(eax, ecx);          
 state[12] = state[12] + eax;    
 eax = state[6];            
 eax = eax & 0x29;         
 edx = state[7];            
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[7] = edx;            
 eax = edi * 0x64f01864;      
 edx = edi;               
 ecx = eax;               
 edx = ROL(edx, ecx);          
 eax = state[16];            
 eax = eax ^ 0x24;         
 ecx = eax;               
 edx = ROR(edx, ecx);          
 edi = edx;               
 eax = state[1];            
 ecx = 0xb;               
 eax = ROL(eax, ecx);          
 edx = state[6];            
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[6] = edx;            

 if (4 == local_u32_0) {        
  ebx = 0x4f3424ed;          
  eax = 0xb8d29f37 + ebx;     
  eax = call(eax, state, 0x915f2d12); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f36ec64;     
  eax = call(ebx, state, 0xa0ce9f8f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd883c86;      
  eax = call(ebx, state, 0xcc0af9dd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a03f388;     
  eax = call(ebx, state, state[13]);  
 }

 edx = state[14];            
 eax = edx;               
 ecx = 0x9;               
 eax = ROR(eax, ecx);          
 edx = edx + eax;          
 state[14] = edx;            
 eax = state[13];            
 eax = eax & 0x24b1abab;      
 eax = eax * state[3];       
 state[3] = eax;            
 eax = eax + 0x10decc67;      
 eax = eax - state[12];       
 state[3] = eax;            
 ebx = state[15];            
 eax = ebx;               
 eax = eax ^ 0x194903b4;      
 edi = edi * eax;          

 if (2 == local_u32_0) {        
  ebx = 0x1644961a;          
  eax = 0xf1c2e8b2 + ebx;     
  eax = call(eax, state, 0x2a8a643e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x354d29f3;     
  eax = call(ebx, state, 0xd325b48f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x712002b1;     
  eax = call(ebx, state, 0xcd35e004); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f5d8d57;     
  eax = call(ebx, state, state[14]);  
  ebx = state[15];           
 }

 eax = ebx * 0x2ed0158e;      
 eax = eax * edi;          
 state[15] = eax;            
 eax = state[14];            
 eax = eax + 0xc4d28c7c;      
 eax = eax + state[3];       
 state[14] = eax;            
 eax = state[18];            
 eax = eax ^ 0x3e1bda7e;      
 state[11] = state[11] - eax;    
 eax = state[13];            
 ecx = 0x18;              
 eax = ROL(eax, ecx);          
 eax = eax * state[2];       
 state[2] = eax;            
 eax = edi;               
 return eax;
}


u32 mix_major7(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 edi = extra_state;           
 eax = state[6];            
 eax = eax ^ state[3];       
 eax = eax ^ edi;          
 local_u32_2 = eax;           
 edx = local_u32_2 % 0x0B;
 local_u32_0 = edx;           
 ecx = edi * 0x25d21c70;      
 state[8] = state[8] + ecx;     
 eax = state[13];            
 ecx = 0x1a;              
 eax = ROR(eax, ecx);          
 edi = edi + eax;          
 eax = state[0];            
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 state[15] = state[15] + eax;    

 if (1 == edx) {            
  ebx = 0x41972847;          
  eax = 0xc67192ad + ebx;     
  eax = call(eax, state, 0x4800fd26); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x903c452;      
  eax = call(ebx, state, 0xc947c99);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x596cfbcc;     
  eax = call(ebx, state, 0xa3f86a33); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30111962;     
  eax = call(ebx, state, state[9]);  
 }

 edx = state[4];            
 edx = edx ^ 0x214bbbb;       
 edi = edi + edx;          
 ebx = edi * 0xffffff9d;      
 eax = state[5];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[5] = eax;            
 edx = state[18];            
 edx = edx | 0x1102e01a;       
 state[17] = state[17] - edx;    
 ecx = state[19];            
 ecx = ecx + 0xf1e0cc5a;      
 ecx = ecx + state[12];       
 state[19] = ecx;            

 if (0 == local_u32_0) {        
  ebx = 0x2ac2dc13;          
  eax = 0xdd4595b1 + ebx;     
  eax = call(eax, state, 0xadec496e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x348650e4;     
  eax = call(ebx, state, 0x40133a42); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4f005cf8;     
  eax = call(ebx, state, 0x97398d49); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4ec6a455;     
  eax = call(ebx, state, state[13]);  
 }

 eax = edi * 0x33ff2ce9;      
 state[8] = state[8] | eax;     
 eax = 0x2fe45acf + edi;      
 eax = eax * state[4];       
 state[4] = eax;            
 eax = edi;               
 ecx = 0xd;               
 eax = ROR(eax, ecx);          
 state[3] = state[3] ^ eax;     
 eax = state[12];            
 eax = eax & 0x2e2ac892;      
 edi = edi ^ eax;          

 if (7 == local_u32_0) {        
  ebx = 0x25f9c71b;          
  eax = 0xe20e1121 + ebx;     
  eax = call(eax, state, 0xeee462ea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11dfb27a;     
  eax = call(ebx, state, 0x98d9073d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x17ba61e5;     
  eax = call(ebx, state, 0x1b928bb0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16de004d;     
  eax = call(ebx, state, state[15]);  
 }

 eax = state[14];            
 ecx = 0x1;               
 eax = ROL(eax, ecx);          
 edi = edi * eax;          
 eax = 0x7a3b4f0e + edi;      
 edi = edi ^ eax;          
 eax = state[11];            
 eax = eax ^ 0x5f050ce6;      
 state[5] = state[5] + eax;     

 if (2 == local_u32_0) {        
  ebx = 0x875632e;           
  eax = 0xff9357ea + ebx;     
  eax = call(eax, state, 0x121beaad); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3ac14255;     
  eax = call(ebx, state, 0x89995d2f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3bd53f06;     
  eax = call(ebx, state, 0x3c456f4e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x98cab54;      
  eax = call(ebx, state, state[17]);  
 }

 eax = state[17];            
 local_u32_1 = eax;           
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 local_u32_3 = eax;           
 eax = state[11];            
 eax = eax & 0x524788df;      
 state[9] = state[9] - eax;     
 edx = local_u32_3;           
 edi = 0x17b2d86 +  edx + edi ;  
 edi = edi + state[3];       
 local_u32_1 = local_u32_1 ^ 0xd2348b5; 
 ecx = local_u32_1;           
 state[12] = state[12] | ecx;    

 if (4 == local_u32_0) {        
  ebx = 0x4effcaca;          
  eax = 0xb908f0de + ebx;     
  eax = call(eax, state, 0x66042b48); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2058ce99;     
  eax = call(ebx, state, 0x71b8deb1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3b96b1b4;     
  eax = call(ebx, state, 0x54d097e1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x399f44c4;     
  eax = call(ebx, state, edi);     
 }

 eax = state[0];            
 eax = eax ^ 0x3ca6760a;      
 state[4] = state[4] + eax;     
 ebx = state[12];            
 ebx = ebx & 0x3e;         
 eax = state[10];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[10] = eax;            
 ebx = edi;               
 ebx = ebx ^ 0x32b59495;      
 state[12] = state[12] - ebx;    
 edx = state[7];            
 edx = edx ^ 0xcc6cef3;       
 state[11] = state[11] - edx;    

 if (9 == local_u32_0) {        
  ebx = 0x74d704d9;          
  eax = 0x932ee617 + ebx;     
  eax = call(eax, state, 0x21a395ed); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x621963df;     
  eax = call(ebx, state, 0x9b3e3072); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xbc48ba0;      
  eax = call(ebx, state, 0x29a09789); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1438d105;     
  eax = call(ebx, state, state[6]);  
 }

 eax = edi;               
 eax = eax ^ 0x42ce4263;      
 state[18] = state[18] - eax;    
 eax = state[15];            
 eax = eax + 0xfc1ccf0a;      
 state[8] = state[8] ^ eax;     
 eax = state[2];            
 eax = eax + 0xdc6ebf0;       
 eax = eax * state[4];       
 state[4] = eax;            

 if (8 == local_u32_0) {        
  ebx = 0x19dcb343;          
  eax = 0xee2acba9 + ebx;     
  eax = call(eax, state, 0x54a43ad9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x345b5487;     
  eax = call(ebx, state, 0x48c51eea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x53857ac7;     
  eax = call(ebx, state, 0xee37aacc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3ad48fa6;     
  eax = call(ebx, state, state[19]);  
 }

 eax = state[17];            
 eax = eax + 0x29e0bfe6;      
 state[14] = state[14] ^ eax;    
 eax = state[0];            
 eax = eax + 0xc0a98770;      
 state[2] = state[2] ^ eax;     
 eax = state[11];            
 ecx = 0xf;               
 eax = ROL(eax, ecx);          
 state[6] = state[6] + eax;     

 if (10 == local_u32_0) {        
  ebx = 0x2285e176;          
  eax = 0xe582912a + ebx;     
  eax = call(eax, state, 0xa5b0fd36); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2040ee60;     
  eax = call(ebx, state, 0xcdef4618); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x21f7bcdc;     
  eax = call(ebx, state, 0x42452ecf); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd5a5e18;      
  eax = call(ebx, state, edi);     
 }

 eax = state[18];            
 edi = 0xff5138a0 +  eax + edi ; 
 edi = edi - state[5];       

 if (6 == local_u32_0) {        
  ebx = 0x13474fab;          
  eax = 0xf4bebded + ebx;     
  eax = call(eax, state, 0xf2b7a1ae); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1296c785;     
  eax = call(ebx, state, 0x4aabda25); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x101be630;     
  eax = call(ebx, state, 0xaf5623);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x54e9d56a;     
  eax = call(ebx, state, edi);     
 }

 ecx = state[17];            
 ecx = ecx & 0x4a;         
 edx = state[3];            
 edx = ROR(edx, ecx);          
 state[3] = edx;            
 eax = state[7] * 0x36e7ec8;    
 edx = edx ^ eax;          
 state[3] = edx;            

 if (3 == local_u32_0) {        
  ebx = 0x1c5510f1;          
  eax = 0xebb360fb + ebx;     
  eax = call(eax, state, 0x58fd7244); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5a399d7d;     
  eax = call(ebx, state, 0x3677750a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x19cf2d26;     
  eax = call(ebx, state, 0x44993f85); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29e84d68;     
  eax = call(ebx, state, edi);     
 }

 ebx = state[19] * 0x23b9e79;    
 eax = state[10];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[10] = eax;            
 edx = state[12];            
 edx = edx + 0xd914afe4;      
 edx = edx * state[14];       
 state[14] = edx;            

 if (5 == local_u32_0) {        
  ebx = 0x21011947;          
  eax = 0xe704c42d + ebx;     
  eax = call(eax, state, 0x4f5f0444); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xede8de4;      
  eax = call(ebx, state, 0x61d0eb96); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x104b666b;     
  eax = call(ebx, state, 0xdb0018de); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x218c1732;     
  eax = call(ebx, state, state[18]);  
 }

 edi = edi + 0xfe572373;      
 edi = edi - state[8];       
 ecx = state[2];            
 ecx = ecx ^ 0x2fe3c8;       
 edi = edi | ecx;          
 eax = state[18];            
 eax = eax + 0x522baa5a;      
 edi = edi ^ eax;          
 edx = state[7];            
 edx = edx ^ 0x1609874e;      
 ecx = state[8];            
 ecx = ecx - edx;          
 state[8] = ecx;            
 eax = state[4];            
 eax = eax | 0x1e171635;       
 edx = state[10];            
 eax = eax ^ edx;          
 state[10] = eax;            
 eax = eax + 0xe985f44f;      
 edi = edi ^ eax;          
 ecx = state[1];            
 ecx = ecx + 0xd304451b;      
 edi = edi ^ ecx;          
 eax = state[6];            
 eax = eax + 0x19b93371;      
 eax = eax - state[16];       
 state[6] = eax;            
 ebx = state[10];            
 ebx = ebx & 0x6d;         
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 return eax;
}


u32 mix_major8(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_5;
 eax = state[5];            
 local_u32_5 = eax;           
 ecx = state[9];            
 local_u32_5 = local_u32_5 ^ ecx;  
 eax = state[19];            
 local_u32_5 = local_u32_5 ^ eax;  
 ecx = local_u32_5 % 0x05;
 local_u32_0 = ecx;           
 edx = state[12];            
 local_u32_5 = edx;           
 local_u32_5 = local_u32_5 ^ 0xb6b4743; 
 eax = local_u32_5;           
 eax = eax + state[5];       
 local_u32_1 = eax;           
 state[5] = eax;            
 edi = 0x221bed03 + edx;      
 edi = edi * extra_state;      
 ecx = state[11];            
 ecx = ecx ^ 0x2663a394;      
 ecx = ecx * state[3];       
 state[3] = ecx;            
 eax = state[4];            
 eax = eax ^ 0x4f1894;       
 edi = edi | eax;          
 ecx = local_u32_1;           
 ecx = ecx & 0xad85e5da;      
 state[5] = ecx;            
 eax = 0xd191e790 + edi;      
 state[17] = state[17] & eax;    
 ecx = state[1] * 0x1c634b75;    
 edi = edi + ecx;          

 if (2 == local_u32_0) {        
  ebx = 0x1c3931ae;          
  eax = 0xebce4d3e + ebx;     
  eax = call(eax, state, 0x636e7b01); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xc20217b;      
  eax = call(ebx, state, 0xe74a1adc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x399c1595;     
  eax = call(ebx, state, 0xd37638);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11a1be7d;     
  eax = call(ebx, state, state[1]);  
 }

 edx = state[6];            
 local_u32_5 = edx;           
 local_u32_5 = local_u32_5 ^ 0x1fdc8171; 
 eax = local_u32_5;           
 state[3] = state[3] - eax;     
 ecx = state[14] * 0xdc63a30;    
 eax = state[15];            
 ecx = ecx ^ eax;          
 local_u32_2 = ecx;           
 state[15] = ecx;            
 ebx = edi;               
 ebx = ebx + 0x28;         
 eax = state[7];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[7] = eax;            
 ebx = state[8];            
 ecx = 0xe4fb2084 + ebx;      
 edi = edi ^ ecx;          
 edx = edx + 0xb6a8bfd8;      
 edx = edx - edi;          
 state[6] = edx;            
 ecx = 0x1;               
 edx = ROR(edx, ecx);          
 edi = edi * edx;          
 ecx = 0x1f;              
 ebx = ROR(ebx, ecx);          
 ebx = ebx * state[13];       
 state[13] = ebx;            
 eax = local_u32_2;           
 eax = eax + 0xa969bc16;      
 state[18] = state[18] ^ eax;    

 if (1 == local_u32_0) {        
  ebx = 0x11614352;          
  eax = 0xf6a4ca26 + ebx;     
  eax = call(eax, state, 0x1453cf27); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xca6a632;      
  eax = call(ebx, state, 0x4a75112a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1527f58b;     
  eax = call(ebx, state, 0x4d8025ff); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1539962f;     
  eax = call(ebx, state, state[2]);  
 }

 edx = state[1];            
 edx = edx & 0x6;          
 eax = edi;               
 ecx = edx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 ecx = state[8];            
 ecx = ecx + 0xeba05ea0;      
 ecx = ecx - state[17];       
 state[8] = ecx;            
 eax = state[16];            
 eax = eax + 0xe8427306;      
 ecx = state[19];            
 eax = eax + ecx;          
 state[16] = eax;            
 edx = state[7];            
 edx = edx + 0x35f9fb28;      
 edx = edx ^ eax;          
 state[16] = edx;            
 edx = edi;               
 edx = edx & 0x16076281;      
 state[13] = state[13] + edx;    
 eax = 0xe43a6120 + edi;      
 edi = edi * eax;          
 ecx = state[1];            
 ecx = ecx + 0xd94074d;       
 ecx = ecx - state[3];       
 state[1] = ecx;            

 if (3 == local_u32_0) {        
  ebx = 0x33c6c1d9;          
  eax = 0xd441afeb + ebx;     
  eax = call(eax, state, 0x170b22b9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5e936df9;     
  eax = call(ebx, state, 0x7ff31123); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f3e6e77;     
  eax = call(ebx, state, 0x940beda);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9ac941f;      
  eax = call(ebx, state, state[17]);  
 }

 edx = state[18];            
 edx = edx + 0xc5;         
 eax = edi;               
 ecx = edx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 ecx = state[6];            
 ecx = ecx + 0x126c7192;      
 ecx = ecx + edi;          
 state[6] = ecx;            
 eax = state[9];            
 eax = eax ^ 0xe4c97d9;       
 state[4] = state[4] & eax;     
 ecx = 0x5246092 + edi;       
 edi = edi ^ ecx;          
 eax = state[14];            
 eax = eax + 0x12466f7c;      
 eax = eax + state[3];       
 state[14] = eax;            
 ecx = state[7];            
 ecx = ecx + 0xe724e487;      
 ecx = ecx - state[19];       
 state[7] = ecx;            
 edi = edi + 0xfffcc68a;      
 edx = state[2];            
 edi = edi - edx;          
 eax = state[12] * 0xf8b6e25;    
 edx = edx - eax;          
 state[2] = edx;            

 if (4 == local_u32_0) {        
  ebx = 0x4cd51c9e;          
  eax = 0xbb32bb36 + ebx;     
  eax = call(eax, state, 0xcc2d355b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd4e5869;      
  eax = call(ebx, state, 0x3daf9351); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x221547e1;     
  eax = call(ebx, state, 0x74973164); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28b16a13;     
  eax = call(ebx, state, state[12]);  
 }

 edx = state[6];            
 edx = edx ^ 0x91;         
 eax = state[3];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[3] = eax;            
 edx = edi;               
 edx = edx & 0x3dd7da06;      
 state[4] = state[4] + edx;     
 edx = state[8];            
 ecx = 0xb6484f2a + edx;      
 ecx = ecx * state[11];       
 state[11] = ecx;            
 edx = edx & 0x274e05b8;      
 edi = edi ^ edx;          
 eax = state[5];            
 eax = eax + 0x263032a4;      
 state[18] = state[18] ^ eax;    
 ecx = 0x1a70ff38 + edi;      
 state[16] = state[16] ^ ecx;    

 if (0 == local_u32_0) {        
  ebx = 0x83c1f57;           
  eax = 0xffcbb8a1 + ebx;     
  eax = call(eax, state, 0x133ea08e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10dde2ef;     
  eax = call(ebx, state, 0xaeffff1d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9b8a9e4;      
  eax = call(ebx, state, 0x62fd21a5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x67498e67;     
  eax = call(ebx, state, state[10]);  
 }

 eax = state[4];            
 eax = eax + 0x4a83a932;      
 eax = eax + edi;          
 local_u32_5 = eax;           
 state[4] = eax;            
 edx = edi;               
 edx = edx ^ 0x1bb7cdc3;      
 ebx = state[2];            
 ebx = ebx + 0xd0;         
 eax = state[19];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[19] = eax;            
 ebx = state[19];            
 ebx = ebx ^ 0xa0;         
 eax = state[0];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[0] = eax;            
 edi = 0xf1efd9b1 +  edx + edi ; 
 edi = edi - local_u32_5;      
 edx = state[1];            
 local_u32_5 = edx;           
 local_u32_5 = local_u32_5 | 0x64a30a; 
 ecx = local_u32_5;           
 state[11] = state[11] ^ ecx;    
 edx = edx + 0x4cd3708;       
 edx = edx - state[8];       
 state[1] = edx;            
 eax = 0xf6d388b6 +  edi + eax ; 
 state[0] = eax;            
 ecx = state[8];            
 ecx = ecx + 0x4b8444f;       
 ecx = ecx - edx;          
 state[8] = ecx;            
 edx = edi;               
 ecx = 0x9;               
 edx = ROL(edx, ecx);          
 eax = state[7];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[7] = eax;            
 edx = state[10];            
 edx = edx + 0x9c;         
 eax = state[17];            
 ecx = edx;               
 eax = ROR(eax, ecx);          
 state[17] = eax;            
 eax = edi;               
 return eax;
}


u32 mix_major9(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2;
 eax = state[15];            
 eax = eax ^ state[1];       
 edx = state[19];            
 eax = eax ^ edx;          
 local_u32_2 = eax;           
 edx = local_u32_2 % 0x0B;
 local_u32_0 = edx;           
 ecx = state[18];            
 ecx = ecx + 0xe56713bc;      
 eax = state[19];            
 eax = eax | ecx;          
 state[19] = eax;            
 edx = state[8];            
 edx = edx + 0xefc639fe;      
 state[12] = state[12] | edx;    
 edi = extra_state;           

 if (2 == local_u32_0) {        
  ebx = 0xc9ad573;           
  eax = 0xfb6d28f9 + ebx;     
  eax = call(eax, state, 0x5d65f9e8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3c086fbb;     
  eax = call(ebx, state, 0xf923e763); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2717d83f;     
  eax = call(ebx, state, 0x3ce5b281); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11b2790a;     
  eax = call(ebx, state, edi);     
 }

 ecx = state[18];            
 ecx = ecx + 0xf20ff41d;      
 state[4] = state[4] ^ ecx;     
 ebx = edi;               
 ebx = ebx + 0xcb;         
 eax = edi;               
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 edi = eax;               

 if (5 == local_u32_0) {        
  ebx = 0x41dda277;          
  eax = 0xc628f2c1 + ebx;     
  eax = call(eax, state, 0xf9d18714); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d16548a;     
  eax = call(ebx, state, 0x56a18cd);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2398a78f;     
  eax = call(ebx, state, 0xedd4970c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1db39ca3;     
  eax = call(ebx, state, edi);     
 }

 eax = edi;               
 ecx = 0x1;               
 eax = ROR(eax, ecx);          
 state[2] = state[2] ^ eax;     
 eax = 0x3842b736 + edi;      
 eax = eax * state[10];       
 state[10] = eax;            

 if (4 == local_u32_0) {        
  ebx = 0x3fd9be7c;          
  eax = 0xc82c18bc + ebx;     
  eax = call(eax, state, 0xdaf55a42); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3bf7979f;     
  eax = call(ebx, state, 0xd1dbb223); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x557953b3;     
  eax = call(ebx, state, 0x6a542aec); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30b11634;     
  eax = call(ebx, state, state[13]);  
 }

 eax = state[5];            
 eax = eax ^ 0x224deca3;      
 eax = eax ^ state[4];       
 state[5] = eax;            
 eax = state[15];            
 eax = eax & 0xe43bfd6;       
 state[9] = state[9] + eax;     
 eax = state[18];            
 eax = eax | 0x24e2f424;       
 state[12] = state[12] + eax;    
 edi = 0xefc5f81f;           

 if (1 == local_u32_0) {        
  ebx = 0x996e178;           
  eax = 0xfe719128 + ebx;     
  eax = call(eax, state, 0x8d4cafe4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x48d5b5c6;     
  eax = call(ebx, state, 0x107e6e17); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4daf021a;     
  eax = call(ebx, state, 0x48a3b120); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x99aa597;      
  eax = call(ebx, state, state[0]);  
 }

 eax = state[10];            
 eax = eax + 0xf0b1e409;      
 eax = eax * state[11];       
 state[11] = eax;            
 edx = state[5];            
 local_u32_1 = edx;           
 edx = edx + 0x2961fc0;       
 state[5] = edx;            
 ecx = local_u32_1;           
 ecx = ecx + 0x13bcdf0b;      
 ecx = ecx * state[9];       
 state[9] = ecx;            
 eax = eax + 0xe91b219c;      
 eax = eax * state[6];       
 state[6] = eax;            

 if (7 == local_u32_0) {        
  ebx = 0xe32932c;           
  eax = 0xf9d4308c + ebx;     
  eax = call(eax, state, 0x226d8967); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3bf9b52c;     
  eax = call(ebx, state, 0x5e9d4724); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x53d9860d;     
  eax = call(ebx, state, 0x7787020);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8dc2a3e;      
  eax = call(ebx, state, edi);     
 }

 eax = state[1];            
 eax = eax + 0xff4abdb4;      
 eax = eax * state[1];       
 state[1] = eax;            
 eax = 0xefc5f81f;           
 ecx = 0x1a;              
 eax = ROL(eax, ecx);          
 edi = 0xefc5f81f + eax;      
 eax = state[10];            
 edx = eax;               
 ecx = 0x1b;              
 edx = ROR(edx, ecx);          
 state[13] = state[13] + edx;    
 eax = eax + state[3];       
 eax = eax + 0xea05fa03;      
 state[10] = eax;            

 if (10 == local_u32_0) {        
  ebx = 0x205e291a;          
  eax = 0xe7a7c52a + ebx;     
  eax = call(eax, state, 0xf92e8b97); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x54a18ff9;     
  eax = call(ebx, state, 0x21976225); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x25daec9f;     
  eax = call(ebx, state, 0xa27b88f5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3390d1c4;     
  eax = call(ebx, state, edi);     
 }

 eax = state[19];            
 eax = eax + 0xe8b6d37d;      
 eax = eax - state[2];       
 state[19] = eax;            
 eax = state[12] * 0xa95c314;    
 edi = edi ^ eax;          

 if (8 == local_u32_0) {        
  ebx = 0x1648c717;          
  eax = 0xf1beb825 + ebx;     
  eax = call(eax, state, 0x4b686b49); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2916e03c;     
  eax = call(ebx, state, 0x848682c);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x57f6b78b;     
  eax = call(ebx, state, 0x2a7e192f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2e044b3e;     
  eax = call(ebx, state, state[11]);  
 }

 eax = state[11];            
 eax = eax & 0x346472bf;      
 edi = edi + eax;          
 eax = state[15] * 0xbeb977c;    
 edi = edi & eax;          
 eax = state[2];            
 eax = eax ^ 0x33dd726a;      
 edi = edi + eax;          
 eax = edi;               
 eax = eax ^ 0x13220e;       
 state[19] = state[19] & eax;    

 if (6 == local_u32_0) {        
  ebx = 0x2263563d;          
  eax = 0xe5a36d5b + ebx;     
  eax = call(eax, state, 0xf012aa11); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x106dcb18;     
  eax = call(ebx, state, 0x2dc38d);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x834d7b4;      
  eax = call(ebx, state, 0x82dd9c35); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3b640f9d;     
  eax = call(ebx, state, state[2]);  
 }

 eax = 0x13a371f7 + edi;      
 edi = edi * eax;          
 ebx = edi * 0xffffffa2;      
 eax = state[0];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[0] = eax;            
 ebx = state[15] * 0x344294f;    
 eax = edi;               
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 edi = eax;               
 edx = state[11];            
 edx = edx | 0x15477725;       
 state[12] = state[12] + edx;    

 if (3 == local_u32_0) {        
  ebx = 0x3aec16e2;          
  eax = 0xcd1aacfa + ebx;     
  eax = call(eax, state, 0x6828082);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe9ee4e5;      
  eax = call(ebx, state, 0x65dea5bd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x377c1e0c;     
  eax = call(ebx, state, 0xe67b2d1);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b731084;     
  eax = call(ebx, state, state[9]);  
 }

 ecx = state[16];            
 ecx = ecx + 0xb2878320;      
 ecx = ecx + state[8];       
 state[16] = ecx;            
 eax = state[11] * 0x128142d3;   
 state[0] = state[0] + eax;     
 ebx = edi;               
 ebx = ebx + 0x49;         
 eax = state[13];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[13] = eax;            

 if (0 == local_u32_0) {        
  ebx = 0x1ce3e4d6;          
  eax = 0xeb2399f6 + ebx;     
  eax = call(eax, state, 0x3299ff38); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3d5b96ed;     
  eax = call(ebx, state, 0x1eba214d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb7d7a74;      
  eax = call(ebx, state, 0xc20441d6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x397dc216;     
  eax = call(ebx, state, state[17]);  
 }

 ebx = state[4];            
 ebx = ebx + 0x3a;         
 eax = state[13];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[13] = eax;            
 edx = 0xb401ddcd + edi;      
 edi = edi | edx;          
 ebx = state[16];            
 ebx = ebx + 0x77;         
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 edx = state[11];            
 edx = edx ^ 0x14302fce;      
 edi = edi + edx;          

 if (9 == local_u32_0) {        
  ebx = 0x36625040;          
  eax = 0xd1a473c0 + ebx;     
  eax = call(eax, state, 0x7adf3317); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xfd596c8;      
  eax = call(ebx, state, 0x1f45c04e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1ff5acb6;     
  eax = call(ebx, state, 0xa60ad5b0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6444281e;     
  eax = call(ebx, state, state[17]);  
 }

 ecx = state[2];            
 ecx = ecx & 0x2104615d;      
 state[7] = state[7] + ecx;     
 eax = state[4];            
 ecx = 0x15;              
 eax = ROL(eax, ecx);          
 state[6] = state[6] | eax;     
 edx = edi * 0x144af0fa;      
 ecx = state[16];            
 ecx = ecx - edx;          
 local_u32_2 = ecx;           
 state[16] = ecx;            
 eax = state[9] * 0x1d7178c2;    
 edi = edi ^ eax;          
 edi = edi * 0x3564b1fd;      
 eax = edi;               
 ecx = 0xb;               
 eax = ROR(eax, ecx);          
 edx = local_u32_2;           
 edx = edx - eax;          
 state[16] = edx;            
 ecx = state[19] * 0x383ae479;   
 state[8] = state[8] ^ ecx;     
 eax = state[11];            
 eax = eax + 0xc4759a85;      
 eax = eax + edi;          
 state[11] = eax;            
 eax = eax + 0x35e01882;      
 eax = eax ^ state[9];       
 state[9] = eax;            
 edx = state[0];            
 edx = edx ^ 0x105d6dd1;      
 state[10] = state[10] & edx;    
 eax = edi;               
 return eax;
}


u32 mix_major10(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 edi = extra_state;           
 edx = state[5] % 0x0B;
 local_u32_0 = edx;           
 ecx = 0x2277a712 + edi;      
 state[17] = state[17] ^ ecx;    
 eax = state[8];            
 eax = eax + 0xe6c6654e;      
 eax = eax * state[19];       
 state[19] = eax;            
 ebx = state[1];            
 ebx = ebx ^ 0x5b;         
 eax = state[6];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[6] = eax;            

 if (3 == edx) {            
  ebx = 0x2cb318a3;          
  eax = 0xdb555991 + ebx;     
  eax = call(eax, state, 0x31a0be43); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c4fa122;     
  eax = call(ebx, state, 0xf1fb17d);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2a4c1510;     
  eax = call(ebx, state, 0xa93e5fab); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x24c59607;     
  eax = call(ebx, state, state[1]);  
 }

 eax = 0x22e5f53d + edi;      
 eax = eax * state[0];       
 state[0] = eax;            
 eax = state[6];            
 eax = eax + 0xf7f0c308;      
 eax = eax - state[14];       
 state[6] = eax;            

 if (6 == local_u32_0) {        
  ebx = 0xef02db5;           
  eax = 0xf917515f + ebx;     
  eax = call(eax, state, 0x83400b93); 
	ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28e5a688;     
  eax = call(ebx, state, 0x42e1981);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xeed8b36;      
  eax = call(ebx, state, 0xce93c93);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe6f7d03;      
  eax = call(ebx, state, state[2]);  
 }

 eax = state[9];            
 edi = 0xafa2e81 +  eax + edi ;  
 eax = state[17];            
 eax = eax + 0xfd2839c0;      
 eax = eax * state[15];       
 state[15] = eax;            
 eax = state[14];            
 eax = eax + 0x30bd8dc6;      
 eax = eax - state[6];       
 state[14] = eax;            
 eax = state[7];            
 eax = eax ^ 0x1edb75c4;      
 state[2] = state[2] + eax;     

 if (4 == local_u32_0) {        
  ebx = 0x947bc72;           
  eax = 0xfec0b532 + ebx;     
  eax = call(eax, state, 0xc4b89afe); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x48c9b95d;     
  eax = call(ebx, state, 0x1d8eda55); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1490ce19;     
  eax = call(ebx, state, 0x48478e22); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x61cfb6c1;     
  eax = call(ebx, state, edi);     
 }

 local_u32_1 = 0x2cfa7327;       
 edx = local_u32_1;           
 state[2] = edx;            
 ecx = state[7];            
 ecx = ecx + 0xf2bf5a7;       
 ecx = ecx - state[8];       
 state[7] = ecx;            
 ebx = state[6];            
 ebx = ebx | 0x95;          
 eax = state[11];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[11] = eax;            
 eax = state[10];            
 ecx = 0x18;              
 eax = ROL(eax, ecx);          
 edx = edx ^ eax;          
 state[2] = edx;            

 if (2 == local_u32_0) {        
  ebx = 0x116e35bf;          
  eax = 0xf697d7d9 + ebx;     
  eax = call(eax, state, 0x92600aeb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x71295806;     
  eax = call(ebx, state, 0x35fb5ef2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2d379e17;     
  eax = call(ebx, state, 0xbc830612); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1ca24534;     
  eax = call(ebx, state, edi);     
 }

 eax = state[5];            
 ecx = 0x1d;              
 eax = ROL(eax, ecx);          
 state[16] = state[16] ^ eax;    
 edx = state[8];            
 eax = edx;               
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[8] = edx;            

 if (0 == local_u32_0) {        
  ebx = 0x2d99ddf6;          
  eax = 0xda6e943e + ebx;     
  eax = call(eax, state, 0x514492f6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xcf44c3d;      
  eax = call(ebx, state, 0x4eb7f6f0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x89185d9;      
  eax = call(ebx, state, 0x420f2116); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1acb56e1;     
  eax = call(ebx, state, edi);     
 }

 ebx = edi;               
 ebx = ebx & 0x9aee05b;       
 ebx = ebx * state[13];       
 state[13] = ebx;            
 ebx = state[0];            
 ebx = ebx + 0x29;         
 eax = state[18];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[18] = eax;            

 if (5 == local_u32_0) {        
  ebx = 0x557494df;          
  eax = 0xb2934319 + ebx;     
  eax = call(eax, state, 0x173d7ef4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14dfe1c5;     
  eax = call(ebx, state, 0x45382ff9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x36db8ef1;     
  eax = call(ebx, state, 0x194b24c);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x246e6736;     
  eax = call(ebx, state, state[5]);  
 }

 eax = state[16];            
 eax = eax + 0x15c7f2a;       
 eax = eax + edi;          
 state[16] = eax;            
 eax = state[8];            
 eax = eax | 0xc568bd;        
 state[0] = state[0] + eax;     
 ebx = state[11];            
 eax = ebx;               
 ecx = 0x19;              
 eax = ROR(eax, ecx);          
 edi = edi + eax;          

 if (10 == local_u32_0) {        
  ebx = 0x38e02ed6;          
  eax = 0xcf2694e2 + ebx;     
  eax = call(eax, state, 0xde2e328);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e7425bb;     
  eax = call(ebx, state, 0x31f4e46c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11e2c1dd;     
  eax = call(ebx, state, 0x65d6d81a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x35e4d51d;     
  eax = call(ebx, state, edi);     
  ebx = state[11];           
 }

 eax = state[0];            
 eax = eax | 0x3c992378;       
 ebx = ebx & eax;          
 state[11] = ebx;            
 edi = edi ^ 0x1ebdf827;      
 eax = state[2];            
 edi = edi ^ eax;          
 edx = state[16];            
 edx = edx & 0x1a8092b;       
 edi = edi ^ edx;          
 eax = eax + 0xf6a7c14d;      
 state[4] = state[4] ^ eax;     

 if (7 == local_u32_0) {        
  ebx = 0xcd8c7c1;           
  eax = 0xfb31212b + ebx;     
  eax = call(eax, state, 0x52f66ce7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e462354;     
  eax = call(ebx, state, 0x27c30f6a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9123123;      
  eax = call(ebx, state, 0x32688671); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf5144ec;      
  eax = call(ebx, state, state[5]);  
 }

 eax = state[1];            
 eax = eax + 0xbd4eb37a;      
 edi = edi | eax;          
 eax = state[15];            
 eax = eax ^ 0xe476c17;       
 edi = edi * eax;          

 if (9 == local_u32_0) {        
  ebx = 0xeb740b3;           
  eax = 0xf9513139 + ebx;     
  eax = call(eax, state, 0x1e3f295b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x147d5f40;     
  eax = call(ebx, state, 0x8b07010);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2aa6178d;     
  eax = call(ebx, state, 0x32a3d5ee); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x100bc99b;     
  eax = call(ebx, state, state[19]);  
 }

 eax = state[4];            
 eax = eax & 0x55d63dde;      
 edx = state[0];            
 edx = edx - eax;          
 state[0] = edx;            
 ecx = state[14];            
 ecx = ecx + 0xfa050d42;      
 ecx = ecx + state[19];       
 state[14] = ecx;            
 edx = edx + 0x9ff4339;       
 eax = state[12];            
 edx = edx & eax;          
 state[12] = edx;            
 edx = edx + 0xccdc186;       
 state[15] = state[15] ^ edx;    

 if (8 == local_u32_0) {        
  ebx = 0xe85826b;           
  eax = 0xf9808b0d + ebx;     
  eax = call(eax, state, 0x8aabd57);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b7bb8d6;     
  eax = call(ebx, state, 0x98b81e75); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3d64fbcc;     
  eax = call(ebx, state, 0x591c3cf9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14a6b9a5;     
  eax = call(ebx, state, state[12]);  
 }

 ebx = state[11];            
 ebx = ebx + 0xfb;         
 eax = state[10];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[10] = eax;            
 edx = state[15];            
 edx = edx + 0x130fea4;       
 state[5] = state[5] ^ edx;     
 ecx = state[19];            
 ecx = ecx + 0xdf1438e7;      
 edi = edi ^ ecx;          

 if (1 == local_u32_0) {        
  ebx = 0x3a630545;          
  eax = 0xcda24683 + ebx;     
  eax = call(eax, state, 0x7cd72ef9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10f4d907;     
  eax = call(ebx, state, 0x142b4efb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1c2e59a2;     
  eax = call(ebx, state, 0xfa8ece76); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16bd9748;     
  eax = call(ebx, state, edi);     
 }

 eax = state[3];            
 eax = eax ^ 0x30f43d2;       
 state[11] = state[11] + eax;    
 edx = state[16];            
 local_u32_3 = edx;           
 ecx = edx * 0x485950f;       
 eax = state[13];            
 eax = eax - ecx;          
 local_u32_2 = eax;           
 state[13] = eax;            
 edx = state[1];            
 edx = edx + 0xa295d0d;       
 edx = edx * state[15];       
 state[15] = edx;            
 ecx = state[0] * 0x68f4b257;    
 edi = edi ^ ecx;          
 eax = state[8];            
 eax = eax + 0xe49d7359;      
 state[12] = state[12] & eax;    
 edx = state[2] * 0x16a7a0b6;    
 state[7] = state[7] - edx;     
 ecx = local_u32_2;           
 ecx = ecx + 0x18727e9f;      
 edi = edi & ecx;          
 eax = edi;               
 ecx = 0x3;               
 eax = ROL(eax, ecx);          
 state[14] = state[14] & eax;    
 edx = state[6];            
 edx = edx ^ 0x13892cf5;      
 state[19] = state[19] - edx;    
 ecx = local_u32_3 * 0x2fad5f96;  
 edi = edi - ecx;          
 eax = edi;               
 return eax;
}


u32 mix_major11(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_1, local_u32_5;
 u32 local_u32_6, local_u32_7, local_u32_8, local_u32_10;
 eax = state[11];            
 eax = eax ^ state[3];       
 ecx = state[17];            
 eax = eax ^ ecx;          
 local_u32_1 = eax;           
 ecx = local_u32_1 % 0x0A;
 local_u32_0 = ecx;           
 eax = state[0];            
 eax = eax & 0x201c33b4;      
 ecx = state[15];            
 ecx = ecx - eax;          
 state[15] = ecx;            
 eax = state[4];            
 eax = eax ^ 0x4b5700f;       
 state[9] = state[9] & eax;     
 ecx = ecx | 0x1f564f3c;       
 edi = extra_state;           
 edi = edi - ecx;          
 edi = edi + 0xfe30d470;      
 edi = edi * state[14];       
 state[14] = edi;            

 if (2 == local_u32_0) {        
  ebx = 0x2a7c09b0;          
  eax = 0xdd89cdac + ebx;     
  eax = call(eax, state, 0xe99287d4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x71f65774;     
  eax = call(ebx, state, 0xac34cb2);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x35e88ffe;     
  eax = call(ebx, state, 0x81b8d9c7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x24fcc448;     
  eax = call(ebx, state, state[1]);  
 }

 eax = state[7];            
 ecx = 0x1c;              
 eax = ROL(eax, ecx);          
 state[3] = state[3] ^ eax;     
 edi = 0xb2363254;           
 state[17] = state[17] + 0x503fc4de; 
 eax = state[1] * 0xf14c9c;     
 state[18] = state[18] + eax;    

 if (6 == local_u32_0) {        
  ebx = 0x19e88d0f;          
  eax = 0xee1d4a29 + ebx;     
  eax = call(eax, state, 0xf698f515); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1f9c2ee8;     
  eax = call(ebx, state, 0x27feb7df); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2b5b3b37;     
  eax = call(ebx, state, 0xdd91fb3);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1561599e;     
  eax = call(ebx, state, state[5]);  
 }

 eax = state[0];            
 eax = eax + 0xaf4b1f37;      
 eax = eax * state[3];       
 local_u32_10 = eax;          
 state[3] = eax;            
 ecx = state[11];            
 ecx = ecx + 0x1d1cbc4e;      
 ecx = ecx * state[11];       
 state[11] = ecx;            
 eax = state[1];            
 eax = eax + 0xf6c6f628;      
 state[13] = state[13] ^ eax;    
 ecx = local_u32_10;          
 ecx = ecx + 0x7f863fa;       
 state[17] = state[17] ^ ecx;    

 if (4 == local_u32_0) {        
  ebx = 0x5730178c;          
  eax = 0xb0d859ec + ebx;     
  eax = call(eax, state, 0x9573ad30); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x114007c7;     
  eax = call(ebx, state, 0xad2eb6f5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10754686;     
  eax = call(ebx, state, 0xdbe4b2f1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x17b3a57e;     
  eax = call(ebx, state, edi);     
 }

 eax = state[4];            
 eax = eax | 0x3b62a700;       
 ecx = state[11];            
 eax = eax + ecx;          
 local_u32_5 = eax;           
 state[11] = eax;            
 state[19] = state[19] ^ 0xf3c3d3f0; 
 edx = state[10];            
 edx = edx + 0xee;         
 eax = 0xb2363254;           
 ecx = edx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 eax = state[16];            
 ecx = 0xa;               
 eax = ROR(eax, ecx);          
 eax = eax | state[16];       
 state[16] = eax;            
 ecx = state[7] * 0x5053948;    
 ecx = ecx * local_u32_5;      
 state[7] = ecx;            

 if (3 == local_u32_0) {        
  ebx = 0xb3a55ce;           
  eax = 0xfccb818e + ebx;     
  eax = call(eax, state, 0xf27bb381); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x68f75e93;     
  eax = call(ebx, state, 0x2e860b98); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1237827d;     
  eax = call(ebx, state, 0x9f5a4b63); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8be3799;      
  eax = call(ebx, state, state[4]);  
 }

 eax = edi * 0x377e5649;      
 state[1] = state[1] & eax;     
 eax = state[2];            
 eax = eax | 0x57a0b91;       
 state[18] = state[18] + eax;    
 edx = edi;               
 edx = edx + 0x87;         
 eax = state[7];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[7] = eax;            
 ecx = 0x2;               
 eax = ROL(eax, ecx);          
 state[4] = state[4] - eax;     

 if (0 == local_u32_0) {        
  ebx = 0xaa2bb66;           
  eax = 0xfd64c3ae + ebx;     
  eax = call(eax, state, 0x77c54c67); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3dea325a;     
  eax = call(ebx, state, 0x4faf0f81); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa4ba223;      
  eax = call(ebx, state, 0x1269df3d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1ac6ffbc;     
  eax = call(ebx, state, state[18]);  
 }

 eax = 0xfea6f980 + edi;      
 edi = edi * eax;          
 ecx = state[2] * 0x33aaef75;    
 state[18] = state[18] + ecx;    
 eax = state[12];            
 eax = eax + 0xda4bd31e;      
 eax = eax ^ state[2];       
 state[2] = eax;            
 ecx = state[6];            
 ecx = ecx | 0x107e370;       
 edi = edi - ecx;          
 eax = state[17];            
 eax = eax + 0x191504c;       
 eax = eax - edi;          
 state[17] = eax;            

 if (9 == local_u32_0) {        
  ebx = 0x459edf9f;          
  eax = 0xc26b0999 + ebx;     
  eax = call(eax, state, 0x1eac1ff2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2744bb63;     
  eax = call(ebx, state, 0xcabc5012); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x51f18344;     
  eax = call(ebx, state, 0x3c6c4607); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x36c913e0;     
  eax = call(ebx, state, state[14]);  
 }

 eax = state[15];            
 ecx = 0x7;               
 eax = ROL(eax, ecx);          
 ecx = state[3];            
 eax = eax + ecx;          
 local_u32_6 = eax;           
 state[3] = eax;            
 eax = state[12];            
 eax = eax + 0x18afd3db;      
 ecx = state[10];            
 eax = eax - ecx;          
 local_u32_7 = eax;           
 state[12] = eax;            
 eax = state[5];            
 eax = eax + 0x1392be9b;      
 eax = eax + local_u32_7;      
 local_u32_6 = local_u32_6 ^ 0xfd205d5; 
 eax = eax - local_u32_6;      
 state[5] = eax;            
 ecx = state[8];            
 ecx = ecx ^ 0x9000ce9;       
 ecx = ecx ^ edi;          
 state[8] = ecx;            

 if (5 == local_u32_0) {        
  ebx = 0x1ef80121;          
  eax = 0xe90de9cf + ebx;     
  eax = call(eax, state, 0x5a3e601c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xebc5abc;      
  eax = call(ebx, state, 0x3350de75); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12c6d39f;     
  eax = call(ebx, state, 0x6f852028); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a65095c;     
  eax = call(ebx, state, state[19]);  
 }

 edx = edi;               
 edx = edx + 0x59;         
 eax = state[19];            
 ecx = edx;               
 eax = ROR(eax, ecx);          
 local_u32_8 = eax;           
 state[19] = eax;            
 ebx = eax * 0x1962fc70;      
 eax = state[11];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[11] = eax;            
 edi = edi ^ 0x534576d7;      
 edi = edi ^ state[12];       
 ecx = state[1];            
 ecx = ecx ^ 0xb5;         
 eax = ROL(eax, ecx);          
 state[11] = eax;            
 eax = state[9] * 0x12af9c5;    
 eax = eax + local_u32_8;      
 state[19] = eax;            

 if (8 == local_u32_0) {        
  ebx = 0x15d62ec1;          
  eax = 0xf23090ef + ebx;     
  eax = call(eax, state, 0x34046fd8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b992eb9;     
  eax = call(ebx, state, 0x9703c6cb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x254e8a41;     
  eax = call(ebx, state, 0x2121c997); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5d401b56;     
  eax = call(ebx, state, state[0]);  
 }

 edx = edi * 0x3533614;       
 eax = state[10];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[10] = eax;            
 eax = state[14];            
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 state[1] = state[1] - eax;     
 ecx = state[16];            
 ecx = ecx + 0xed222733;      
 edi = edi | ecx;          
 eax = state[3] * 0x532f53a;    
 eax = eax & state[16];       
 state[16] = eax;            
 ecx = state[11] * 0x14718f9a;   
 edi = edi ^ ecx;          

 if (1 == local_u32_0) {        
  ebx = 0xb4d804d;           
  eax = 0xfcb9f8ab + ebx;     
  eax = call(eax, state, 0x2365f8b7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2e27ec1b;     
  eax = call(ebx, state, 0xd06aae75); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x266f0c54;     
  eax = call(ebx, state, 0xf17ef298); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2480bb8c;     
  eax = call(ebx, state, state[13]);  
 }

 eax = edi;               
 eax = eax | 0x1739a522;       
 eax = eax * state[3];       
 state[3] = eax;            
 eax = state[1];            
 eax = eax | 0x4b09e3e;       
 edi = edi * eax;          
 eax = state[7];            
 eax = eax ^ 0x2a4ea48a;      
 eax = eax ^ state[12];       
 state[7] = eax;            

 if (7 == local_u32_0) {        
  ebx = 0x6209b092;          
  eax = 0xa5fdcece + ebx;     
  eax = call(eax, state, 0xcc87cc91); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x58c6c39b;     
  eax = call(ebx, state, 0xd484f14b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30c61335;     
  eax = call(ebx, state, 0xd566f896); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b7712a6;     
  eax = call(ebx, state, edi);     
 }

 eax = state[19];            
 eax = eax + 0x1dc54aa;       
 eax = eax - edi;          
 state[19] = eax;            
 eax = edi;               
 return eax;
}


u32 mix_major12(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0;
 edi = extra_state;           
 ebx = edi;               
 ebx = ebx ^ state[7];       
 ebx = ebx ^ state[16];       
 ebx = ebx % 0x06;
 local_u32_0 = ebx;           
 ecx = state[18];            
 ecx = ecx & 0x104394c4;      
 edx = state[6];            
 ecx = ecx & edx;          
 state[18] = ecx;            
 eax = 0xe92519e2 + edi;      
 edi = edi * eax;          
 ecx = state[4];            
 ecx = ecx + 0x46d5ad23;      
 ecx = ecx + state[19];       
 state[4] = ecx;            
 edx = edx + state[1];       
 edx = edx + 0x3fd0884;       
 state[6] = edx;            
 edi = edi + 0xc46fe68;       
 edi = edi * state[9];       
 state[9] = edi;            
 ebx = state[13];            

 if (5 == local_u32_0) {        
  ebx = 0x19e50541;          
  eax = 0xee236cf3 + ebx;     
  eax = call(eax, state, 0x55da4384); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x80b66cb;      
  eax = call(ebx, state, 0x3cb08f38); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xcc589bf;      
  eax = call(ebx, state, 0xbb73ac5);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11925158;     
  eax = call(ebx, state, state[4]);  
  ebx = state[13];           
 }

 eax = state[11];            
 eax = eax ^ 0x4453b1d7;      
 edx = state[7];            
 eax = eax ^ edx;          
 state[11] = eax;            
 eax = state[12];            
 eax = eax + 0x187596ce;      
 state[4] = state[4] ^ eax;     
 eax = state[19];            
 eax = eax ^ 0x1ecd4347;      
 state[14] = state[14] + eax;    
 eax = state[6];            
 eax = eax + 0xaa504a66;      
 state[17] = state[17] & eax;    
 eax = 0x2482f7ba + ebx;      
 eax = eax - edx;          
 state[13] = eax;            

 if (2 == local_u32_0) {        
  ebx = 0xc26638e;           
  eax = 0xfbe20eca + ebx;     
  eax = call(eax, state, 0xc3898ffa); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1083e5c3;     
  eax = call(ebx, state, 0x38e82473); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x82a5ad4;      
  eax = call(ebx, state, 0xd79d054);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa25a651;      
  eax = call(ebx, state, state[18]);  
 }

 edx = state[17];            
 edx = edx | 0x14128b1f;       
 edx = edx * state[5];       
 state[5] = edx;            
 eax = state[9];            
 eax = eax | 0x8ae69ec;       
 eax = eax & edx;          
 state[5] = eax;            
 eax = eax | 0x25dcee2a;       
 edi = eax * 0xf7abca44;      
 edx = state[10];            
 eax = edx * 0x2b5c108a;      
 state[12] = state[12] + eax;    
 eax = state[19];            
 eax = eax + 0x45d1e08;       
 eax = eax - edx;          
 state[19] = eax;            

 if (1 == local_u32_0) {        
  ebx = 0x33109515;          
  eax = 0xd4f4df53 + ebx;     
  eax = call(eax, state, 0x3ded2d48); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb1d4f29;      
  eax = call(ebx, state, 0x27f9409b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x93be110;      
  eax = call(ebx, state, 0x2bcf3aae); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12559492;     
  eax = call(ebx, state, state[7]);  
 }

 eax = state[5];            
 eax = eax + 0x17a9626b;      
 eax = eax - state[3];       
 state[5] = eax;            
 ecx = state[8];            
 edi = 0x55003f14 +  ecx + edi ; 
 eax = state[6];            
 ecx = 0x1f;              
 eax = ROL(eax, ecx);          
 state[9] = state[9] + eax;     
 edx = state[19];            
 ecx = 0xd;               
 edx = ROL(edx, ecx);          
 state[2] = state[2] | edx;     
 eax = state[19];            
 eax = eax ^ 0xfbf02d6;       
 ecx = state[15];            
 eax = eax ^ ecx;          
 state[19] = eax;            
 edx = state[18] * 0x279ed38c;   
 edx = edx | state[3];        
 state[3] = edx;            
 eax = eax ^ 0x234a2088;      
 edi = edi & eax;          

 if (0 == local_u32_0) {        
  ebx = 0x2bc2170a;          
  eax = 0xdc465b76 + ebx;     
  eax = call(eax, state, 0x22f3cb4e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x15530bf0;     
  eax = call(ebx, state, 0x9406e46c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2044022d;     
  eax = call(ebx, state, 0xa520b878); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x76355cf7;     
  eax = call(ebx, state, state[14]);  
 }

 edx = state[4];            
 edx = edx + 0xd5555942;      
 edx = edx + state[9];       
 state[4] = edx;            
 eax = state[6];            
 eax = eax + 0xf6a829d0;      
 eax = eax + state[0];       
 state[6] = eax;            
 eax = state[17] * 0x6877a2b6;   
 state[2] = state[2] + eax;     
 eax = state[11];            
 eax = eax + 0x4f92882e;      
 edi = edi | eax;          
 eax = 0x2a0e1a7a + edi;      
 edx = edx ^ eax;          
 state[4] = edx;            
 eax = edi * 0xba88b94;       
 edi = edi * eax;          

 if (3 == local_u32_0) {        
  ebx = 0x6123c069;          
  eax = 0xa6e48c4b + ebx;     
  eax = call(eax, state, 0x10a0821a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3294f0c3;     
  eax = call(ebx, state, 0x12f626b0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16d16ff7;     
  eax = call(ebx, state, 0x10d0c58f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b926ed4;     
  eax = call(ebx, state, edi);     
 }

 eax = state[19];            
 eax = eax ^ 0x88fae5c;       
 state[8] = state[8] - eax;     
 eax = edi;               
 eax = eax ^ 0x6171e1a;       
 edi = edi - eax;          
 eax = state[0];            
 eax = eax & 0x6369ab7c;      
 edi = edi * eax;          
 eax = state[12];            
 eax = eax & 0x36b79ddb;      
 state[2] = state[2] ^ eax;     
 eax = 0xff3ba490 + edi;      
 edi = edi ^ eax;          

 if (4 == local_u32_0) {        
  ebx = 0x1fa22572;          
  eax = 0xe8664d0e + ebx;     
  eax = call(eax, state, 0x9ee6d639); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x85d1ae0;      
  eax = call(ebx, state, 0x5bbacf6c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe7e5302;      
  eax = call(ebx, state, 0x42775467); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1fa6b102;     
  eax = call(ebx, state, state[2]);  
 }

 edx = state[9];            
 eax = edx * 0x2a0582f6;      
 edi = edi ^ eax;          
 eax = state[10];            
 eax = eax + 0xf71f2197;      
 edx = edx ^ eax;          
 state[9] = edx;            
 eax = 0x417b0639 + edi;      
 state[17] = state[17] | eax;    
 eax = edi;               
 ecx = 0x11;              
 eax = ROR(eax, ecx);          
 state[6] = state[6] ^ eax;     
 eax = state[15];            
 eax = eax + 0x1935355;       
 eax = eax - state[3];       
 state[15] = eax;            
 eax = eax ^ 0x232ddb67;      
 edi = edi - eax;          
 eax = edi;               
 ecx = 0x5;               
 eax = ROR(eax, ecx);          
 edi = edi ^ eax;          
 eax = state[13];            
 eax = eax + 0x25393a1;       
 eax = eax + state[5];       
 state[13] = eax;            
 eax = edi;               
 return eax;
}


u32 mix_major13(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_2, local_u32_3;
 edi = extra_state;           
 eax = state[1];            
 eax = eax ^ state[18];       
 edx = state[12];            
 eax = eax ^ edx;          
 local_u32_2 = eax;           
 edx = local_u32_2 % 0x0B;
 local_u32_0 = edx;           
 ecx = 0xfd2296dd + edi;      
 ecx = ecx * state[7];       
 state[7] = ecx;            
 eax = state[9];            
 eax = eax + 0x10ce1e6b;      
 edi = edi * eax;          
 edx = state[14];            
 edx = edx & 0xe7aa887;       
 state[13] = state[13] | edx;    

 if (9 == local_u32_0) {        
  ebx = 0x11e19413;          
  eax = 0xf623e055 + ebx;     
  eax = call(eax, state, 0x7e09a7f0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4bc0c591;     
  eax = call(ebx, state, 0x57f23dd8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x19400a71;     
  eax = call(ebx, state, 0x5f161e78); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3fc74508;     
  eax = call(ebx, state, state[15]);  
 }

 eax = state[19];            
 eax = eax + 0x44864e65;      
 edx = state[17];            
 eax = eax + edx;          
 state[19] = eax;            
 eax = state[2];            
 eax = eax + 0x456501d3;      
 eax = eax - state[10];       
 state[2] = eax;            
 edx = edx + 0xe91158ed;      
 state[11] = state[11] ^ edx;    

 if (6 == local_u32_0) {        
  ebx = 0x2051e037;          
  eax = 0xe7b691b5 + ebx;     
  eax = call(eax, state, 0x1fa49c4e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13138297;     
  eax = call(ebx, state, 0x232ac1d8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9b045e2;      
  eax = call(ebx, state, 0xf3117cd);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x7a23c229;     
  eax = call(ebx, state, state[8]);  
 }

 eax = state[13];            
 eax = eax + 0xffeafe84;      
 eax = eax - edi;          
 state[13] = eax;            
 eax = state[10];            
 eax = eax & 0x5898bbff;      
 state[3] = state[3] ^ eax;     
 eax = state[17];            
 eax = eax ^ 0xb4b5ddd;       
 edi = edi - eax;          
 eax = 0xf2a69347 + edi;      
 state[5] = state[5] & eax;     

 if (7 == local_u32_0) {        
  ebx = 0x13a2c8dc;          
  eax = 0xf465a934 + ebx;     
  eax = call(eax, state, 0x1f9b5706); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3b35def6;     
  eax = call(ebx, state, 0x459d2894); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x559c045b;     
  eax = call(ebx, state, 0x201209ea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xaa1e6e9;      
  eax = call(ebx, state, state[19]);  
 }

 eax = state[8];            
 eax = eax + 0x35a3f082;      
 eax = eax + state[11];       
 state[8] = eax;            
 eax = 0xf0918e1c + edi;      
 state[15] = state[15] & eax;    

 if (8 == local_u32_0) {        
  ebx = 0x280acf6c;          
  eax = 0xdffdeb60 + ebx;     
  eax = call(eax, state, 0x8bd80373); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x148bc40e;     
  eax = call(ebx, state, 0xd9974fd1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2541882a;     
  eax = call(ebx, state, 0x75b8e9a8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x15fad807;     
  eax = call(ebx, state, edi);     
 }

 edi = edi + 0x1e87b29e;      
 edi = edi - state[12];       
 eax = state[0];            
 eax = eax + 0x9b993250;      
 edi = edi ^ eax;          
 eax = state[17] * 0xb083b2b;    
 state[13] = state[13] ^ eax;    

 if (5 == local_u32_0) {        
  ebx = 0x170f36ae;          
  eax = 0xf0f99c82 + ebx;     
  eax = call(eax, state, 0x26543385); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b3d5b2c;     
  eax = call(ebx, state, 0xe94e46f9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e2913f3;     
  eax = call(ebx, state, 0x5a87491f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x373366db;     
  eax = call(ebx, state, state[14]);  
 }

 ebx = state[0];            
 ebx = ebx ^ 0xfa;         
 eax = state[1];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[1] = eax;            
 edx = state[11] * 0x17321349;   
 state[5] = state[5] ^ edx;     
 ecx = state[3];            
 ecx = ecx + 0xffce689b;      
 edi = edi ^ ecx;          
 eax = 0x2570be6e + edi;      
 eax = eax * state[4];       
 state[4] = eax;            

 if (10 == local_u32_0) {        
  ebx = 0x370669bf;          
  eax = 0xd1025199 + ebx;     
  eax = call(eax, state, 0xc0e125f2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x60242a4d;     
  eax = call(ebx, state, 0xb0589fc5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5fbfcb5e;     
  eax = call(ebx, state, 0xd1e831cb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6dd0763d;     
  eax = call(ebx, state, state[6]);  
 }

 eax = state[15] * 0x2d42b937;   
 state[15] = eax;            
 eax = 0xf544478e + edi;      
 eax = eax * state[4];       
 state[4] = eax;            
 eax = state[9];            
 eax = eax ^ 0x4dc36a;       
 eax = eax + state[0];       
 eax = eax + 0x10bb4f25;      
 eax = eax - edi;          
 state[0] = eax;            

 if (3 == local_u32_0) {        
  ebx = 0x3803000d;          
  eax = 0xd004de33 + ebx;     
  eax = call(eax, state, 0x3c79ee8);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x187149a3;     
  eax = call(ebx, state, 0xe7de0565); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1726fb8c;     
  eax = call(ebx, state, 0x43816fc4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x18cac331;     
  eax = call(ebx, state, state[15]);  
 }

 eax = state[3];            
 ecx = 0xe;               
 eax = ROR(eax, ecx);          
 state[19] = state[19] & eax;    
 eax = state[17] * 0x18575b09;   
 eax = eax * edi;          
 state[17] = eax;            
 eax = edi * 0x50ebe77;       
 state[1] = state[1] | eax;     
 eax = state[6];            
 eax = eax | 0x4d24003d;       
 edi = edi + eax;          

 if (4 == local_u32_0) {        
  ebx = 0x12323aeb;          
  eax = 0xf5d6806d + ebx;     
  eax = call(eax, state, 0x49d6a047); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb4b7c79;      
  eax = call(ebx, state, 0x3f12d53c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30a1cc8f;     
  eax = call(ebx, state, 0x1103fedb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x974ca10;      
  eax = call(ebx, state, state[9]);  
 }

 eax = state[0];            
 edx = 0xf770857b + eax;      
 state[15] = state[15] & edx;    
 edx = edi * 0xffffffed;      
 ecx = edx;               
 eax = ROR(eax, ecx);          
 state[0] = eax;            
 eax = edi;               
 eax = eax | 0x2576a843;       
 edi = edi - eax;          

 if (0 == local_u32_0) {        
  ebx = 0xafd038b;           
  eax = 0xfd0a756d + ebx;     
  eax = call(eax, state, 0x5d3f9c2);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xcbf9bc4;      
  eax = call(ebx, state, 0x261400df); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1c6413fc;     
  eax = call(ebx, state, 0x4a39e439); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x35f7e08e;     
  eax = call(ebx, state, state[8]);  
 }

 eax = edi * 0x2994c8c;       
 state[1] = state[1] + eax;     
 edx = state[6];            
 edx = edx + 0xfe25a480;      
 state[16] = state[16] ^ edx;    
 ecx = state[3] * 0x1e333f7b;    
 ecx = ecx * state[11];       
 state[3] = ecx;            
 ebx = state[17];            
 ebx = ebx ^ 0xda;         
 eax = state[7];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[7] = eax;            

 if (2 == local_u32_0) {        
  ebx = 0xe593a75;           
  eax = 0xf9acb07b + ebx;     
  eax = call(eax, state, 0xd62dffe0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8b92eff;      
  eax = call(ebx, state, 0xc6acc5fa); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x186c4cc6;     
  eax = call(ebx, state, 0x1f4370a1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a13c311;     
  eax = call(ebx, state, state[1]);  
 }

 eax = state[18];            
 eax = eax + 0x149e5b40;      
 state[13] = state[13] ^ eax;    
 eax = state[0];            
 eax = eax + 0x541a494;       
 eax = eax + state[19];       
 state[0] = eax;            

 if (1 == local_u32_0) {        
  ebx = 0x27bdda2b;          
  eax = 0xe049fdf1 + ebx;     
  eax = call(eax, state, 0x16a6c65a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x429bee24;     
  eax = call(ebx, state, 0xbc669be);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf21b844;      
  eax = call(ebx, state, 0xec378400); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8be815c;      
  eax = call(ebx, state, edi);     
 }

 ebx = state[2];            
 ebx = ebx + 0x16deeae;       
 ebx = ebx - edi;          
 state[2] = ebx;            
 edx = state[0];            
 edx = edx ^ 0x1120ce2d;      
 ecx = state[9];            
 ecx = ecx - edx;          
 state[9] = ecx;            
 eax = state[13];            
 eax = eax ^ 0x2a74ac2a;      
 edx = state[7];            
 eax = eax ^ edx;          
 local_u32_3 = eax;           
 state[13] = eax;            
 ecx = ecx + 0xdab80c67;      
 state[12] = state[12] & ecx;    
 ecx = edi * 0x2776477;       
 state[14] = state[14] - ecx;    
 eax = state[19] * 0x2f2e21d0;   
 state[4] = state[4] - eax;     
 edx = state[19];            
 edx = edx + 0xe78ae13d;      
 edx = edx - state[3];       
 state[19] = edx;            
 local_u32_2 = edi;           
 local_u32_2 = local_u32_2 ^ 0x434c0d3a; 
 edi = edi - local_u32_2;      
 edi = edi + 0x11f70706;      
 edi = edi - ebx;          
 ebx = state[16];            
 ebx = ebx + 0xa9;         
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 edx = edi * 0x2a0d21c3;      
 ecx = local_u32_3;           
 ecx = ecx + edx;          
 state[13] = ecx;            
 return eax;
}


u32 mix_major14(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi;
 u32 local_u32_0, local_u32_2, local_u32_3;
 edi = extra_state;           
 eax = state[6];            
 eax = eax ^ state[8];       
 edx = state[15];            
 eax = eax ^ edx;          
 local_u32_2 = eax;           
 edx = local_u32_2 % 0x0B;
 local_u32_0 = edx;           
 ebx = edi;               
 ebx = ebx ^ 0x1c0b5143;      
 state[14] = state[14] & ebx;    
 ecx = state[5];            
 ecx = ecx + 0x4ef38b53;      
 ecx = ecx * state[17];       
 state[17] = ecx;            
 eax = state[16];            
 ecx = 0x8;               
 eax = ROR(eax, ecx);          
 edx = state[15];            
 edx = edx ^ eax;          
 state[15] = edx;            

 if (4 == local_u32_0) {        
  ebx = 0xd6108f9;           
  eax = 0xfaa5b733 + ebx;     
  eax = call(eax, state, 0x8018c7d6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6fe1d683;     
  eax = call(ebx, state, 0x1e59fb72); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x472a7554;     
  eax = call(ebx, state, 0x389a9392); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x32354cff;     
  eax = call(ebx, state, state[10]);  
 }

 ecx = state[17];            
 ecx = ecx & 0x3b118c17;      
 edi = edi ^ ecx;          
 ebx = state[7] * 0xb755cab;    
 eax = edi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 eax = state[12];            
 ecx = 0x5;               
 eax = ROR(eax, ecx);          
 state[5] = state[5] - eax;     

 if (10 == local_u32_0) {        
  ebx = 0xf0419ef;           
  eax = 0xf904a169 + ebx;     
  eax = call(eax, state, 0x89c9204a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xba12ab1;      
  eax = call(ebx, state, 0x84049032); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4a03a834;     
  eax = call(ebx, state, 0x75f250e0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3b410335;     
  eax = call(ebx, state, edi);     
 }

 eax = state[10];            
 eax = eax + 0xe81a232b;      
 edi = edi ^ eax;          
 eax = state[2];            
 eax = eax + 0xef9e8d77;      
 state[18] = state[18] | eax;    
 eax = state[3];            
 eax = eax + 0xce3d3234;      
 eax = eax + state[4];       
 state[3] = eax;            

 if (5 == local_u32_0) {        
  ebx = 0x821cfa2;           
  eax = 0xffe4c572 + ebx;     
  eax = call(eax, state, 0x32dff049); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28fb5aa0;     
  eax = call(ebx, state, 0xa26e4bf7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11608876;     
  eax = call(ebx, state, 0x71b10f8b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe16838d;      
  eax = call(ebx, state, state[0]);  
 }

 eax = edi;               
 ecx = 0xf;               
 eax = ROR(eax, ecx);          
 edi = edi * eax;          
 eax = state[7];            
 eax = eax + 0x358107b;       
 edi = edi & eax;          
 eax = state[3];            
 ecx = 0x14;              
 eax = ROL(eax, ecx);          
 state[12] = state[12] + eax;    

 if (3 == local_u32_0) {        
  ebx = 0xd4c6b32;           
  eax = 0xfab8e096 + ebx;     
  eax = call(eax, state, 0x85b2703b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a1793fd;     
  eax = call(ebx, state, 0x14ef3da3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3b55263f;     
  eax = call(ebx, state, 0x5aef9557); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x234312bc;     
  eax = call(ebx, state, edi);     
 }

 edi = 0xddcb6fb3 +  edi + edi ; 
 eax = state[4];            
 edx = eax * 0x2a5c35ea;      
 edi = edi ^ edx;          
 eax = eax + 0x3b4034a1;      
 eax = eax - state[3];       
 state[4] = eax;            
 eax = state[19];            
 eax = eax | 0x2856103;       
 state[11] = state[11] & eax;    

 if (1 == local_u32_0) {        
  ebx = 0x3390e971;          
  eax = 0xd474edc7 + ebx;     
  eax = call(eax, state, 0xd1bc3e1f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x32c0f296;     
  eax = call(ebx, state, 0xb9b5e313); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x83901c8e;     
  eax = call(ebx, state, 0xe5a8a731); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3e52815c;     
  eax = call(ebx, state, state[16]);  
 }

 eax = 0x2d3d686 + edi;       
 state[7] = state[7] | eax;     
 edi = edi & 0x316de5b2;      
 edi = edi & state[15];       

 if (7 == local_u32_0) {        
  ebx = 0x2c48e5ea;          
  eax = 0xdbbc65de + ebx;     
  eax = call(eax, state, 0x745b1705); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x880f7bf;      
  eax = call(ebx, state, 0x1b46a1af); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2604f26c;     
  eax = call(ebx, state, 0x1d71d16c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x400b87df;     
  eax = call(ebx, state, state[15]);  
 }

 edi = edi ^ 0x3e8999a9;      
 edi = edi ^ state[17];       
 edi = 0x4d77c09e +  edi + edi ; 
 eax = state[10];            
 eax = eax + 0xd1650ad7;      
 eax = eax * state[6];       
 state[6] = eax;            
 eax = state[3];            
 eax = eax & 0xade0835;       
 eax = eax * state[7];       
 state[7] = eax;            

 if (0 == local_u32_0) {        
  ebx = 0x2b0ea976;          
  eax = 0xdcf7eb7e + ebx;     
  eax = call(eax, state, 0xfac9a580); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10108a81;     
  eax = call(ebx, state, 0x11883d98); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12ce1052;     
  eax = call(ebx, state, 0xcb5e054f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x426d3606;     
  eax = call(ebx, state, state[3]);  
 }

 eax = state[15];            
 eax = eax ^ 0x32bd1767;      
 ecx = state[9];            
 ecx = ecx - eax;          
 state[9] = ecx;            
 eax = state[3];            
 eax = eax + 0x74289e8a;      
 eax = eax ^ state[12];       
 state[12] = eax;            
 edx = state[5];            
 edx = edx + 0xd55d1b86;      
 ecx = ecx ^ edx;          
 state[9] = ecx;            
 eax = eax * 0x13b7b134;      
 edi = edi & eax;          

 if (8 == local_u32_0) {        
  ebx = 0x119dd064;          
  eax = 0xf669aefc + ebx;     
  eax = call(eax, state, 0xf85a7826); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14e8fcac;     
  eax = call(ebx, state, 0xd87f8252); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x124b6021;     
  eax = call(ebx, state, 0xecfa4ab0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x9fcef9d;      
  eax = call(ebx, state, state[2]);  
 }

 edi = 0xda1b9ad7 +  edi + edi ; 
 eax = state[18] * 0x452ad09;    
 state[6] = state[6] - eax;     
 eax = edi;               
 eax = eax ^ 0x4895c9e2;      
 state[4] = state[4] + eax;     

 if (9 == local_u32_0) {        
  ebx = 0xc8e3513;           
  eax = 0xfb7943e5 + ebx;     
  eax = call(eax, state, 0x46d5ff5c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x49b8a062;     
  eax = call(ebx, state, 0x7e0da31e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x71122e44;     
  eax = call(ebx, state, 0x47d92940); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6681a0a2;     
  eax = call(ebx, state, state[16]);  
 }

 eax = 0xf8ecf928 + edi;      
 edi = edi ^ eax;          
 ebx = state[5];            
 ebx = ebx + 0xad;         
 eax = state[18];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[18] = eax;            

 if (6 == local_u32_0) {        
  ebx = 0x12b224c1;          
  eax = 0xf557c453 + ebx;     
  eax = call(eax, state, 0x3de045ea); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xdf84a69;      
  eax = call(ebx, state, 0x8fc579af); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29918481;     
  eax = call(ebx, state, 0xf71d5ad3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16abef26;     
  eax = call(ebx, state, state[8]);  
 }

 edi = edi * 0x34b70af0;      
 eax = state[19];            
 ecx = 0x17;              
 eax = ROL(eax, ecx);          
 state[5] = state[5] - eax;     

 if (2 == local_u32_0) {        
  ebx = 0x222129c9;          
  eax = 0xe5e7914f + ebx;     
  eax = call(eax, state, 0x2ac07a2c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x139cdfae;     
  eax = call(ebx, state, 0x39fb10b8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2098837f;     
  eax = call(ebx, state, 0x13d373);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c4227ef;     
  eax = call(ebx, state, state[18]);  
 }

 eax = state[5];            
 ecx = 0x2;               
 eax = ROR(eax, ecx);          
 eax = eax * state[8];       
 local_u32_2 = eax;           
 state[8] = eax;            
 local_u32_3 = eax;           
 local_u32_3 = local_u32_3 & 0x15595f; 
 edx = state[17];            
 local_u32_3 = local_u32_3 + edx;  
 ecx = local_u32_3;           
 state[17] = ecx;            
 ebx = state[7];            
 ebx = ebx + 0x41;         
 eax = state[19];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[19] = eax;            
 edx = edi * 0x539f549;       
 state[9] = state[9] - edx;     
 ecx = local_u32_2;           
 ecx = ecx ^ 0x10549d01;      
 ecx = ecx * state[0];       
 local_u32_2 = ecx;           
 state[0] = ecx;            
 eax = state[4];            
 eax = eax ^ 0x1cd38676;      
 state[11] = state[11] - eax;    
 eax = edi;               
 ecx = 0x10;              
 eax = ROR(eax, ecx);          
 state[12] = state[12] + eax;    
 ebx = state[16];            
 ecx = 0x1b;              
 ebx = ROL(ebx, ecx);          
 eax = edi;               
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 edi = eax;               
 edx = state[15];            
 edx = edx + 0x266b587;       
 local_u32_3 = local_u32_3 ^ edx;  
 eax = local_u32_2;           
 ecx = 0x1d;              
 eax = ROR(eax, ecx);          
 edx = local_u32_3;           
 edx = edx - eax;          
 state[17] = edx;            
 ecx = state[3];            
 ecx = ecx + 0x2669d0a1;      
 ecx = ecx - state[13];       
 state[3] = ecx;            
 eax = edi;               
 return eax;
}


u32 mix_major15(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_3, local_u32_5;
 edx = state[15];            
 local_u32_1 = edx;           
 ecx = extra_state;           
 local_u32_0 = ecx;           
 local_u32_0 = local_u32_0 ^ edx;  
 edx = state[12];            
 local_u32_0 = local_u32_0 ^ edx;  
 local_u32_0 = local_u32_0 & 0x3;  
 eax = state[3];            
 eax = eax ^ 0x94;         
 edx = state[6];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 eax = edx;               
 state[6] = eax;            
 ebx = extra_state;           
 ebx = ebx ^ 0x9a94557;       
 ebx = ebx + state[12];       
 state[12] = ebx;            
 ebx = state[11] * 0xfffffff9;   
 edx = extra_state;           
 ecx = ebx;               
 edx = ROL(edx, ecx);          
 eax = eax ^ 0x2c63c7d7;      
 eax = eax * local_u32_1;      
 state[15] = eax;            
 eax = state[9] * 0x3471499e;    
 edx = edx - eax;          
 eax = state[4];            
 eax = eax + 0x1565237b;      
 eax = eax - state[17];       
 state[4] = eax;            
 edx = edx ^ 0x34293622;      
 ecx = state[3];            
 edx = edx ^ ecx;          
 extra_state = edx;           
 ecx = state[11];            
 ecx = 0xbab1970a +  edx + ecx ; 
 state[11] = ecx;            
 eax = state[18];            
 eax = eax & 0x2e7cbf50;      
 state[7] = state[7] | eax;     

 if (2 == local_u32_0) {        
  ebx = 0xb3d04b4;           
  eax = 0xfcc99084 + ebx;     
  eax = call(eax, state, 0x27ea8ce3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2b71b273;     
  eax = call(ebx, state, 0x5041e568); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6079bfd3;     
  eax = call(ebx, state, 0x51614109); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xc8ede28;      
  eax = call(ebx, state, state[11]);  
 }

 ebx = state[12];            
 esi = 0xc178e538 + ebx;      
 esi = esi & state[16];       
 state[16] = esi;            
 edx = state[6] * 0xf7a199;     
 ecx = state[14];            
 edx = edx | ecx;          
 local_u32_5 = edx;           
 state[14] = edx;            
 edx = state[9];            
 edx = edx + 0x598a281;       
 edx = edx + extra_state;      
 local_u32_3 = edx;           
 state[9] = edx;            
 eax = state[0];            
 eax = eax + 0xf6c67dcd;      
 extra_state = extra_state ^ eax;  
 ebx = ebx * 0x2a688905;      
 ebx = ebx + local_u32_5;      
 state[14] = ebx;            
 eax = extra_state;           
 eax = eax | 0x29;          
 edx = esi;               
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[16] = edx;            
 eax = extra_state;           
 eax = eax | 0x4d8cb855;       
 state[10] = state[10] + eax;    
 eax = state[19];            
 eax = eax + 0x32b94292;      
 eax = eax - local_u32_3;      
 state[19] = eax;            
 eax = local_u32_3 * 0x1c873f09;  
 edx = extra_state;           
 ecx = eax;               
 edx = ROR(edx, ecx);          
 eax = state[4] * 0x1643c5e0;    
 ecx = eax;               
 edx = ROL(edx, ecx);          
 extra_state = edx;           

 if (1 == local_u32_0) {        
  ebx = 0x58c55d76;          
  eax = 0xaf42219e + ebx;     
  eax = call(eax, state, 0xed396fb0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe755d43;      
  eax = call(ebx, state, 0x9bac9a5a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x7539af42;     
  eax = call(ebx, state, 0x861ab0ce); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13e11494;     
  edx = extra_state;          
  eax = call(ebx, state, edx);     
 }

 eax = state[10];            
 ecx = 0x1c;              
 eax = ROL(eax, ecx);          
 state[6] = state[6] & eax;     
 ebx = extra_state;           
 ebx = ebx ^ 0x5aafcd4a;      
 ebx = ebx + state[16];       
 state[16] = ebx;            
 eax = extra_state;           
 eax = eax ^ 0x1c22a3b7;      
 state[12] = state[12] & eax;    
 eax = state[17];            
 eax = eax ^ 0x2e;         
 edx = extra_state;           
 ecx = eax;               
 edx = ROL(edx, ecx);          
 eax = state[4] * 0x358b021d;    
 ecx = state[18];            
 ecx = ecx - eax;          
 local_u32_5 = ecx;           
 state[18] = ecx;            
 eax = state[13];            
 eax = eax + 0xac30f7a;       
 ebx = ebx ^ eax;          
 state[16] = ebx;            
 eax = state[1];            
 eax = eax + 0xc2;         
 ecx = eax;               
 edx = ROR(edx, ecx);          
 extra_state = edx;           
 edx = local_u32_5;           
 edx = edx + 0xee6e38da;      
 edx = edx - extra_state;      
 state[18] = edx;            

 if (0 == local_u32_0) {        
  ebx = 0x22f1fe09;          
  eax = 0xe514c61b + ebx;     
  eax = call(eax, state, 0x3ebe9185); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xbc54210;      
  eax = call(ebx, state, 0x4fe7d66);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x941115c;      
  eax = call(ebx, state, 0x2c1aad5f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xdba0a7f;      
  eax = call(ebx, state, state[9]);  
 }

 eax = state[16];            
 eax = eax | 0x5cbeb00;       
 state[2] = state[2] + eax;     
 eax = extra_state;           
 ecx = 0x16;              
 eax = ROR(eax, ecx);          
 esi = state[7];            
 esi = esi - eax;          
 state[7] = esi;            
 eax = extra_state;           
 eax = eax + 0x1580fb54;      
 state[4] = state[4] ^ eax;     
 ebx = state[12];            
 eax = ebx;               
 ecx = 0x19;              
 eax = ROL(eax, ecx);          
 edx = state[17];            
 edx = edx - eax;          
 local_u32_5 = edx;           
 state[17] = edx;            
 eax = state[8];            
 eax = eax ^ 0x1b3ea2;       
 eax = eax + state[16];       
 state[16] = eax;            
 eax = state[5];            
 eax = eax + 0x193cf230;      
 eax = eax - extra_state;      
 state[5] = eax;            
 eax = extra_state;           
 eax = eax + 0x72;         
 edx = state[18];            
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[18] = edx;            
 eax = local_u32_5;           
 eax = eax & 0x66e0e812;      
 extra_state = extra_state - eax;  
 eax = esi;               
 ecx = 0x12;              
 eax = ROL(eax, ecx);          
 ebx = ebx ^ eax;          
 state[12] = ebx;            
 edx = local_u32_5;           
 edx = edx + 0xb70d1a;       
 edx = edx - state[13];       
 state[17] = edx;            

 if (3 == local_u32_0) {        
  ebx = 0x8f38237;           
  eax = 0xff1538e1 + ebx;     
  eax = call(eax, state, 0x13a2c225); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3a392dfd;     
  eax = call(ebx, state, 0x537936c7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd90e540;      
  eax = call(ebx, state, 0x339ba318); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12f073aa;     
  eax = call(ebx, state, state[17]);  
 }

 eax = state[6];            
 eax = eax + 0xdfef3914;      
 eax = eax + state[1];       
 state[6] = eax;            
 eax = state[5];            
 ecx = 0x1d;              
 eax = ROL(eax, ecx);          
 extra_state = extra_state + eax;  
 eax = state[8];            
 eax = eax | 0x456bd4b;       
 state[18] = state[18] - eax;    
 eax = state[13];            
 edx = 0x123e07ad + eax;      
 extra_state = extra_state & edx;  
 ecx = extra_state * 0x22af60a0;  
 state[0] = state[0] ^ ecx;     
 eax = eax + 0xf69f7aa2;      
 eax = eax - state[12];       
 state[13] = eax;            
 ebx = extra_state;           
 ebx = ebx ^ 0xfc;         
 edx = state[17];            
 ecx = ebx;               
 edx = ROR(edx, ecx);          
 state[17] = edx;            
 ecx = state[5] * 0x248bf14b;    
 eax = eax + ecx;          
 state[13] = eax;            
 eax = extra_state;           
 ecx = 0xc;               
 eax = ROR(eax, ecx);          
 state[2] = state[2] ^ eax;     
 eax = extra_state;           
 return eax;
}


u32 mix_major16(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 u32 local_u32_4, local_u32_5;
 esi = extra_state;           
 ecx = state[12] % 0x03;
 local_u32_0 = ecx;           
 eax = state[7];            
 eax = eax + 0x1256f342;      
 state[7] = state[7] ^ eax;     
 eax = state[14];            
 ecx = 0x9;               
 eax = ROL(eax, ecx);          
 state[9] = state[9] ^ eax;     
 ecx = state[13];            
 local_u32_4 = ecx;           
 local_u32_4 = local_u32_4 ^ 0x4a20925; 
 eax = local_u32_4;           
 state[0] = state[0] + eax;     
 ecx = esi;               
 ecx = ecx | 0xab;          
 eax = state[13];            
 eax = ROR(eax, ecx);          
 state[13] = eax;            
 esi = esi + 0x2cd8307e;      
 ecx = state[10];            
 local_u32_1 = ecx;           
 esi = esi - ecx;          
 ebx = state[17] * 0x5979375;    
 eax = esi;               
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 esi = eax;               
 ecx = state[15];            
 ecx = ecx | 0x11570bca;       
 state[8] = state[8] + ecx;     
 eax = state[3];            
 eax = eax ^ 0x4c404c71;      
 esi = esi & eax;          
 local_u32_1 = local_u32_1 ^ 0x85d82e; 
 esi = esi + local_u32_1;      
 ecx = state[6];            
 ecx = ecx & 0xf076b8f;       
 ecx = ecx * state[11];       
 eax = 0x26d0f98c +  esi + ecx ; 
 state[11] = eax;            

 if (0 == local_u32_0) {        
  ebx = 0x53615f60;          
  eax = 0xb4a56458 + ebx;     
  eax = call(eax, state, 0x3d9e7fb0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2e262bc7;     
  eax = call(ebx, state, 0x1dcab3d);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x22e1d224;     
  eax = call(ebx, state, 0xb736039);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1632b7a2;     
  eax = call(ebx, state, state[7]);  
 }

 eax = esi;               
 ecx = 0x17;              
 eax = ROR(eax, ecx);          
 state[1] = state[1] ^ eax;     
 ecx = state[9];            
 ecx = ecx + 0xf24cc80b;      
 ecx = ecx + esi;          
 state[9] = ecx;            
 edx = state[14] * 0x6223b3d;    
 eax = state[3];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[3] = eax;            
 ecx = state[19];            
 ecx = ecx + 0x64922cc;       
 ecx = ecx + esi;          
 state[19] = ecx;            
 esi = esi + 0x1e0944e3;      
 esi = esi - state[0];       
 edx = esi * 0xffffffdc;      
 eax = esi;               
 ecx = edx;               
 eax = ROR(eax, ecx);          
 esi = eax;               
 ecx = state[15];            
 ecx = ecx + 0x8d90c5a3;      
 esi = esi * ecx;          
 ebx = esi;               
 ebx = ebx & 0xdd9bf1a;       
 ebx = ebx ^ state[17];       
 state[17] = ebx;            
 eax = state[4];            
 eax = eax + 0xd5bd8bc1;      
 eax = eax - state[6];       
 state[4] = eax;            
 esi = 0x1226f462 +  esi + esi ; 
 eax = state[13];            
 ecx = 0x5;               
 eax = ROL(eax, ecx);          
 ebx = ebx ^ eax;          
 state[17] = ebx;            
 ecx = esi;               
 ecx = ecx & 0x12;         
 eax = state[13];            
 eax = ROL(eax, ecx);          
 state[13] = eax;            

 if (2 == local_u32_0) {        
  ebx = 0x30c6c674;          
  eax = 0xd741f480 + ebx;     
  eax = call(eax, state, 0xd9ef397d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x450a5550;     
  eax = call(ebx, state, 0x3407a08);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xfdfe561;      
  eax = call(ebx, state, 0xf1979b41); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14646414;     
  eax = call(ebx, state, esi);     
 }

 esi = esi | 0x10b9b57a;       
 esi = esi | state[9];        
 ecx = state[0];            
 ecx = ecx + 0x477a65c2;      
 eax = state[10];            
 ecx = ecx + eax;          
 local_u32_2 = ecx;           
 state[0] = ecx;            
 ecx = state[7];            
 local_u32_4 = ecx;           
 local_u32_4 = local_u32_4 ^ 0x1b348ba1; 
 eax = local_u32_4;           
 state[8] = state[8] | eax;     
 ebx = state[1];            
 ecx = 0x8;               
 ebx = ROL(ebx, ecx);          
 eax = state[16];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[16] = eax;            
 ebx = state[19];            
 ecx = ebx * 0xfa375c5;       
 esi = esi ^ ecx;          
 eax = state[5];            
 ecx = 0xd;               
 eax = ROR(eax, ecx);          
 local_u32_4 = eax;           
 eax = state[11];            
 ecx = local_u32_4;           
 eax = ROR(eax, ecx);          
 state[11] = eax;            
 ecx = 0x64bd3f85 + ebx;      
 ecx = ecx ^ state[7];       
 state[7] = ecx;            
 eax = esi;               
 ecx = 0x19;              
 eax = ROR(eax, ecx);          
 eax = eax * state[6];       
 local_u32_3 = eax;           
 state[6] = eax;            
 ecx = state[5];            
 ecx = 0xaeeb67de +  esi + ecx ; 
 local_u32_5 = ecx;           
 state[5] = ecx;            
 eax = ecx;               
 ecx = 0x16;              
 eax = ROR(eax, ecx);          
 ebx = ebx | eax;          
 state[19] = ebx;            
 ecx = local_u32_2;           
 eax = local_u32_3;           
 ecx = 0xe1f2872 +  eax + ecx ;  
 state[0] = ecx;            

 if (1 == local_u32_0) {        
  ebx = 0x7ae257b5;          
  eax = 0x8d278b43 + ebx;     
  eax = call(eax, state, 0x4b192186); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4bd4d32b;     
  eax = call(ebx, state, 0x5af38d8a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3c981448;     
  eax = call(ebx, state, 0xd6e998c5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13822b7e;     
  eax = call(ebx, state, esi);     
 }

 esi = esi | 0x40c95dca;       
 esi = esi | state[6];        
 ebx = state[12];            
 ebx = ebx ^ 0xe1;         
 eax = esi;               
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 esi = eax;               
 ecx = state[8];            
 ecx = ecx + 0xed5ca98b;      
 state[3] = state[3] & ecx;     
 eax = state[4];            
 eax = eax + 0x92abec6e;      
 eax = eax + esi;          
 state[4] = eax;            
 eax = state[13];            
 ecx = 0x16;              
 eax = ROR(eax, ecx);          
 esi = esi & eax;          
 ecx = state[15] * 0xff635ec;    
 state[2] = state[2] + ecx;     
 eax = 0x37343841 + esi;      
 eax = eax ^ state[6];       
 state[6] = eax;            
 ecx = state[9];            
 ecx = ecx + 0xf8e12c69;      
 ecx = ecx + state[14];       
 state[9] = ecx;            
 eax = state[10];            
 ecx = 0x14;              
 eax = ROL(eax, ecx);          
 ecx = state[14];            
 ecx = ecx - eax;          
 state[14] = ecx;            
 eax = esi;               
 return eax;
}


u32 mix_major17(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 u32 local_u32_8, local_u32_7, local_u32_6, local_u32_5;
 esi = extra_state;           
 local_u32_0 = esi;           
 local_u32_0 = local_u32_0 & 0x1;  
 edx = state[5];            
 edx = edx + 0x34b87873;      
 edx = edx - state[18];       
 state[5] = edx;            
 ecx = state[17];            
 ecx = ecx + 0x2051ec4;       
 ebx = state[1];            
 local_u32_1 = ebx;           
 ecx = ecx - ebx;          
 local_u32_2 = ecx;           
 state[17] = ecx;            
 ecx = state[6];            
 ecx = ecx ^ 0x5c80bc7;       
 ebx = state[16];            
 local_u32_8 = ebx;           
 ecx = ecx ^ ebx;          
 local_u32_3 = ecx;           
 state[6] = ecx;            
 eax = esi;               
 ecx = 0x1a;              
 eax = ROR(eax, ecx);          
 esi = esi - eax;          
 eax = ebx;               
 eax = eax | 0x154e9813;       
 eax = eax * edx;          
 state[5] = eax;            
 eax = eax + 0xbac2a47e;      
 state[0] = state[0] | eax;     
 eax = state[9];            
 eax = eax ^ 0xbf263a6;       
 eax = eax * state[13];       
 state[13] = eax;            
 edx = state[11];            
 ecx = 0x17;              
 edx = ROL(edx, ecx);          
 edx = edx | state[9];        
 state[9] = edx;            
 local_u32_1 = local_u32_1 & 0x1c28de84; 
 ebx = ebx * local_u32_1;      
 state[16] = ebx;            
 eax = state[2];            
 ecx = 0xb;               
 eax = ROR(eax, ecx);          
 eax = eax ^ local_u32_3;      
 state[6] = eax;            
 eax = edx;               
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 eax = eax ^ state[12];       
 state[12] = eax;            
 esi = 0x2c5a0200 +  esi + esi ; 
 eax = eax + 0xa104f7f6;      
 state[19] = state[19] | eax;    
 ebx = state[11];            
 ebx = ebx + 0xf51e9043;      
 ebx = ebx ^ local_u32_2;      
 state[17] = ebx;            
 eax = state[15];            
 eax = eax + 0x37f1bc89;      
 eax = eax + esi;          
 state[15] = eax;            

 if (0 == local_u32_0) {        
  ebx = 0x2aad9557;          
  eax = 0xdd5adc21 + ebx;     
  eax = call(eax, state, 0x1ab3f2a6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x229df58f;     
  eax = call(ebx, state, 0xdd3520d7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xcbadb94;      
  eax = call(ebx, state, 0x14421a21); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x70789b6e;     
  eax = call(ebx, state, state[4]);  
 }

 eax = state[5];            
 eax = eax | 0x79ba9a48;       
 esi = esi + eax;          
 eax = state[2];            
 eax = eax ^ 0x1ecdadba;      
 ecx = state[4];            
 ecx = ecx - eax;          
 state[4] = ecx;            
 eax = state[10];            
 eax = eax + 0xf01ca4cf;      
 esi = esi ^ eax;          
 eax = state[8];            
 eax = eax + 0xf58222aa;      
 esi = esi ^ eax;          
 edx = state[7];            
 eax = edx * 0x59c62257;      
 eax = eax | state[8];        
 state[8] = eax;            
 eax = edx;               
 eax = eax | 0x2d2750f0;       
 edx = edx ^ eax;          
 state[7] = edx;            
 eax = state[17];            
 eax = eax | 0x1719d4f;       
 esi = esi + eax;          
 ecx = ecx + 0xcec35bec;      
 ecx = ecx * state[19];       
 state[19] = ecx;            
 eax = state[2];            
 eax = eax + 0xdc17a237;      
 state[18] = state[18] ^ eax;    
 ebx = state[5];            
 ebx = 0xca0f8bc5 +  ebx + ecx ; 
 state[19] = ebx;            
 esi = 0xff282d98 +  esi + esi ; 
 eax = state[0];            
 eax = eax + 0x2a09f2a5;      
 eax = eax + esi;          
 state[0] = eax;            
 ecx = state[2];            
 ecx = ecx + 0x30e437d6;      
 state[11] = state[11] ^ ecx;    
 eax = 0xee36df26 + esi;      
 state[12] = state[12] | eax;    
 eax = 0xc95e1442 + esi;      
 state[15] = state[15] & eax;    

 if (1 == local_u32_0) {        
  ebx = 0x2d247be9;          
  eax = 0xdae2192b + ebx;     
  eax = call(eax, state, 0x90fb3a37); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4444c17e;     
  eax = call(ebx, state, 0xd54c23bc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14a145fd;     
  eax = call(ebx, state, 0xdb72a68e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf0042e7;      
  eax = call(ebx, state, state[14]);  
 }

 edx = state[17];            
 eax = edx;               
 eax = eax ^ 0x72eeed7;       
 ebx = state[7];            
 ebx = ebx - eax;          
 eax = ebx;               
 state[7] = eax;            
 edx = edx * 0x162a030d;      
 ecx = state[15];            
 local_u32_5 = ecx;           
 edx = edx * ecx;          
 state[17] = edx;            
 edx = state[14];            
 edx = edx + 0xf0dd3ef3;      
 edx = edx & eax;          
 state[7] = edx;            
 ebx = state[1];            
 local_u32_7 = ebx;           
 eax = ebx;               
 ecx = ecx ^ ecx;          
 eax = ROL(eax, ecx);          
 esi = esi + eax;          
 eax = state[2];            
 eax = eax ^ 0x2d9ceb17;      
 eax = eax ^ state[13];       
 state[2] = eax;            
 local_u32_6 = esi;           
 local_u32_6 = local_u32_6 ^ 0x176b1b8e; 
 local_u32_6 = local_u32_6 & edx;  
 ebx = local_u32_6;           
 state[7] = ebx;            
 eax = 0xdab13e76 + esi;      
 state[8] = state[8] | eax;     
 edx = state[16];            
 edx = edx + 0x2a74b8d4;      
 edx = edx - state[12];       
 state[16] = edx;            
 esi = esi + 0xcc1039a3;      
 esi = esi - state[4];       
 eax = state[5] * 0x1239378b;    
 esi = esi - eax;          
 eax = state[0];            
 eax = eax ^ 0xd9a5ac4;       
 eax = eax ^ esi;          
 state[0] = eax;            
 local_u32_7 = local_u32_7 ^ 0x346ff630; 
 eax = state[10];            
 eax = eax - local_u32_7;      
 state[10] = eax;            
 esi = esi & 0x5638016d;      
 esi = esi & state[4];       
 eax = eax + 0xa4c7df2;       
 esi = esi & eax;          
 local_u32_5 = local_u32_5 ^ 0x2f99340b; 
 ecx = state[14];            
 ecx = ecx + local_u32_5;      
 state[14] = ecx;            
 eax = local_u32_6;           
 eax = eax + 0xd5881b85;      
 state[11] = state[11] | eax;    
 eax = edx * 0x474eb79;       
 ebx = state[9];            
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 state[9] = ebx;            
 eax = esi;               
 return eax;
}


u32 mix_major18(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_3;
 eax = state[17];            
 eax = eax ^ state[16];       
 edx = state[13];            
 eax = eax ^ edx;          
 local_u32_1 = eax;           
 esi = local_u32_1 % 0x07;
 ecx = state[2];            
 ecx = ecx + 0xe7e9ac84;      
 ecx = ecx - state[9];       
 state[2] = ecx;            
 eax = extra_state;           
 eax = eax + 0xd5e47036;      
 edx = state[7];            
 eax = eax & edx;          
 state[7] = eax;            
 eax = eax ^ 0x5d5e7006;      
 eax = eax ^ state[18];       
 state[7] = eax;            
 ecx = state[6];            
 ecx = ecx ^ 0x16afd25f;      
 extra_state = extra_state + ecx;  
 ebx = state[18];            
 ebx = ebx | 0x7b;          
 eax = state[0];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[0] = eax;            

 if (4 == esi) {            
  ebx = 0x965ca89;           
  eax = 0xfea1b443 + ebx;     
  eax = call(eax, state, 0x62cbb3b);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10ee0a10;     
  eax = call(ebx, state, 0xc4eceecd); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1124f940;     
  eax = call(ebx, state, 0xebfb8088); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x326c2e23;     
  eax = call(ebx, state, state[17]);  
 }

 eax = state[1] * 0x927384a;    
 eax = eax * state[0];       
 state[1] = eax;            
 ebx = state[6];            
 eax = ebx * 0x2ac0b63c;      
 extra_state = extra_state ^ eax;  
 eax = state[5] * 0xef44412;    
 extra_state = extra_state ^ eax;  
 eax = state[18];            
 ecx = 0x16;              
 eax = ROL(eax, ecx);          
 extra_state = extra_state - eax;  

 if (1 == esi) {            
  ebx = 0x519fd660;          
  ebx = ebx + 0xb6680198;     
  eax = call(ebx, state, 0x17300251); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14f0c17e;     
  eax = call(ebx, state, 0xa78ae46);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x170799d1;     
  eax = call(ebx, state, 0xfb09d5f3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x163a2f6d;     
  eax = extra_state;          
  eax = call(ebx, state, eax);     
  ebx = state[6];           
 }

 edx = extra_state;           
 edx = edx + 0x4d05da6a;      
 edx = edx & ebx;          
 local_u32_0 = edx;           
 state[6] = edx;            
 ecx = state[18];            
 ecx = ecx ^ 0xe2ba11c;       
 ecx = ecx * state[13];       
 state[13] = ecx;            
 extra_state = extra_state ^ 0x2e3d328f; 
 eax = state[2];            
 extra_state = extra_state ^ eax;  
 edx = state[1];            
 edx = edx | 0x110c8a1;       
 edx = edx * extra_state;      
 extra_state = edx;           
 ebx = local_u32_0;           
 ecx = 0x1b;              
 ebx = ROR(ebx, ecx);          
 eax = state[4];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[4] = eax;            

 if (0 == esi) {            
  ebx = 0x2a26800c;          
  ebx = ebx + 0xdde1f16c;     
  eax = call(ebx, state, 0xc38b9c69); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xd1177f5;      
  eax = call(ebx, state, 0x14b81240); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f130e33;     
  eax = call(ebx, state, 0x48654651); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x17363e9a;     
  edx = extra_state;          
  eax = call(ebx, state, edx);     
 }

 eax = extra_state;           
 ecx = 0x8;               
 eax = ROR(eax, ecx);          
 eax = eax & state[19];       
 state[19] = eax;            
 ecx = 0xc;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state | eax;  
 eax = state[14] * 0x2d8924b3;   
 extra_state = extra_state + eax;  
 eax = state[15];            
 eax = eax + 0xdcba6126;      
 state[10] = state[10] ^ eax;    
 eax = state[16];            
 eax = eax & 0xf72e29a;       
 extra_state = extra_state + eax;  
 eax = state[18];            
 eax = eax | 0x7614cfb;       
 state[3] = state[3] - eax;     

 if (6 == esi) {            
  ebx = 0x4bcccf35;          
  eax = 0xbc3bec73 + ebx;     
  eax = call(eax, state, 0x18c653bc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x461b7329;     
  eax = call(ebx, state, 0x395f45a4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b8d8c45;     
  eax = call(ebx, state, 0x534a55df); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5e09f2b0;     
  eax = call(ebx, state, state[9]);  
 }

 edx = state[4];            
 eax = 0xfe6ea18f + edx;      
 state[19] = state[19] & eax;    
 eax = state[7];            
 eax = eax & 0x226185b2;      
 eax = eax * state[6];       
 state[6] = eax;            
 edx = edx ^ 0x35388017;      
 state[0] = state[0] + edx;     
 eax = state[14] * 0x268d6eae;   
 extra_state = extra_state ^ eax;  

 if (3 == esi) {            
  ebx = 0x1f921322;          
  eax = 0xe8763992 + ebx;     
  eax = call(eax, state, 0x285b4910); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb909f91;      
  eax = call(ebx, state, 0x552ba7b1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8da6f58;      
  eax = call(ebx, state, 0xc846609e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x29012dff;     
  eax = call(ebx, state, state[0]);  
 }

 eax = extra_state;           
 local_u32_0 = eax;           
 local_u32_0 = local_u32_0 ^ 0xbf3b8c0; 
 edx = local_u32_0;           
 state[15] = state[15] + edx;    
 ebx = state[18];            
 ecx = 0x19;              
 ebx = ROR(ebx, ecx);          
 eax = state[10];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[10] = eax;            
 edx = extra_state;           
 local_u32_0 = edx;           
 local_u32_0 = local_u32_0 ^ 0x61d2180; 
 ecx = state[19];            
 local_u32_0 = local_u32_0 | ecx;  
 eax = local_u32_0;           
 state[19] = eax;            
 eax = eax + 0x588d79a3;      
 state[4] = state[4] & eax;     

 if (5 == esi) {            
  ebx = 0xe28c906;           
  eax = 0xf9dfa90a + ebx;     
  eax = call(eax, state, 0xbdf382e1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x164003e9;     
  eax = call(ebx, state, 0xccb8542b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d35c128;     
  eax = call(ebx, state, 0x82e42b57); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x43d2d955;     
  eax = call(ebx, state, state[8]);  
 }

 edx = state[0];            
 edx = edx + 0x19039f88;      
 edx = edx + extra_state;      
 state[0] = edx;            
 ebx = state[7];            
 ecx = 0xe;               
 ebx = ROR(ebx, ecx);          
 eax = extra_state;           
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 extra_state = eax;           
 edx = state[8];            
 edx = edx ^ 0x1f3dce4;       
 ecx = state[6];            
 edx = edx + ecx;          
 local_u32_1 = edx;           
 state[6] = edx;            
 eax = state[18];            
 eax = eax + 0x4f2cb877;      
 eax = eax * state[17];       
 state[17] = eax;            
 edx = state[15] * 0x177f5d63;   
 ecx = local_u32_1;           
 ecx = ecx & edx;          
 state[6] = ecx;            
 ebx = state[16];            
 ecx = 0x1;               
 ebx = ROL(ebx, ecx);          
 eax = state[12];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[12] = eax;            

 if (2 == esi) {            
  ebx = 0x1a7f7f66;          
  eax = 0xed865e5a + ebx;     
  eax = call(eax, state, 0xd14f3dcf); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4c6201aa;     
  eax = call(ebx, state, 0xc921ff3a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f18b656;     
  eax = call(ebx, state, 0xc2793931); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x27f5be4e;     
  eax = call(ebx, state, state[18]);  
 }

 esi = state[19];            
 esi = esi + 0xbe9fd027;      
 ebx = state[12];            
 esi = esi + ebx;          
 state[19] = esi;            
 edx = state[2] * 0x3ec8c5cb;    
 extra_state = extra_state & edx;  
 ecx = state[4];            
 local_u32_0 = ecx;           
 local_u32_0 = local_u32_0 & 0x48357b75; 
 eax = local_u32_0;           
 state[8] = state[8] + eax;     
 edx = state[6];            
 edx = edx + 0xf4;         
 eax = state[1];            
 ecx = edx;               
 eax = ROL(eax, ecx);          
 state[1] = eax;            
 edx = state[11];            
 edx = edx + 0x13c7dc0f;      
 ecx = state[14];            
 edx = edx ^ ecx;          
 local_u32_3 = edx;           
 state[14] = edx;            
 eax = extra_state;           
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 eax = eax + state[4];       
 state[4] = eax;            
 ebx = ebx + 0x15ea2e80;      
 ebx = ebx - state[2];       
 state[12] = ebx;            
 edx = state[11];            
 esi = 0xaff84c32 +  esi + edx ; 
 state[11] = esi;            
 ebx = extra_state;           
 ebx = ebx ^ 0x58dd1776;      
 ebx = ebx ^ state[7];       
 ecx = state[5] * 0x278991a8;    
 eax = state[2];            
 ecx = ecx ^ eax;          
 state[2] = ecx;            
 edx = local_u32_3;           
 edx = 0xf431b0d4 +  ecx + edx ; 
 state[14] = edx;            
 eax = ebx;               
 return eax;
}


u32 mix_major19(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi, esi;
 u32 local_u32_0, local_u32_2, local_u32_3;
 u32 local_u32_21, local_u32_22, local_u32_23, local_u32_9;
 u32 local_u32_24, local_u32_11, local_u32_25, local_u32_12;
 u32 local_u32_26, local_u32_13, local_u32_14, local_u32_15;
 u32 local_u32_16, local_u32_17, local_u32_28, local_u32_18;
 u32 local_u32_19, local_u32_20, local_u32_4, local_u32_10;
 u32 local_u32_6, local_u32_7, local_u32_8, local_u32_5;
 edi = extra_state;           
 ecx = 0xd2670e69 + edi;      
 ebx = state[3];            
 ecx = ecx ^ ebx;          
 local_u32_0 = ecx;           
 state[3] = ecx;            
 ecx = state[2];            
 local_u32_21 = ecx;          
 eax = ecx;               
 eax = eax & 0x3bd91a6d;      
 edi = edi ^ eax;          
 esi = state[12];            
 edi = 0xe162a863 +  esi + edi ; 
 ebx = local_u32_0;           
 ebx = ebx + 0x2f72a89a;      
 ebx = ebx - edi;          
 state[3] = ebx;            
 ebx = ebx & 0x4053f57a;      
 ecx = state[11];            
 ebx = ebx ^ ecx;          
 local_u32_2 = ebx;           
 state[11] = ebx;            
 eax = 0xfe64a9df + esi;      
 edi = edi * eax;          
 ebx = state[6];            
 ebx = ebx ^ 0x6c235a3;       
 ecx = state[14];            
 local_u32_22 = ecx;          
 ebx = ebx ^ ecx;          
 local_u32_3 = ebx;           
 state[6] = ebx;            
 eax = state[7];            
 eax = eax + 0x7b;         
 ebx = local_u32_2;           
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 local_u32_4 = ebx;           
 state[11] = ebx;            
 ebx = state[8];            
 local_u32_23 = ebx;          
 ebx = ebx + 0xdf869976;      
 ecx = state[13];            
 ebx = ebx ^ ecx;          
 local_u32_5 = ebx;           
 state[13] = ebx;            
 eax = state[12];            
 eax = eax + 0x41;         
 ebx = state[4];            
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 local_u32_6 = ebx;           
 state[4] = ebx;            
 ebx = local_u32_5;           
 local_u32_7 = ebx;           
 local_u32_7 = local_u32_7 ^ 0x3d475dc2; 
 ecx = state[9];            
 local_u32_7 = local_u32_7 + ecx;  
 ebx = local_u32_7;           
 state[9] = ebx;            
 eax = edi;               
 ecx = 0x19;              
 eax = ROR(eax, ecx);          
 eax = eax + local_u32_22;     
 local_u32_8 = eax;           
 state[14] = eax;            
 local_u32_9 = edi;           
 local_u32_9 = local_u32_9 ^ 0x222fef6f; 
 ecx = state[10];            
 local_u32_9 = local_u32_9 + ecx;  
 ecx = local_u32_9;           
 state[10] = ecx;            
 ebx = state[19];            
 local_u32_24 = ebx;          
 ecx = 0x9;               
 ebx = ROR(ebx, ecx);          
 ebx = ebx ^ esi;          
 local_u32_10 = ebx;          
 state[12] = ebx;            
 ebx = local_u32_6;           
 ecx = local_u32_23;          
 ebx = 0x56d964ed +  ecx + ebx ; 
 local_u32_11 = ebx;          
 state[4] = ebx;            
 ebx = state[18];            
 ebx = ebx + 0x132444b;       
 ecx = state[7];            
 local_u32_25 = ecx;          
 ebx = ebx - ecx;          
 local_u32_12 = ebx;          
 state[18] = ebx;            
 state[0] = 0xd35add1b;         
 eax = local_u32_3 * 0x6fe2b2f;   
 edi = edi + eax;          
 ebx = state[1];            
 local_u32_26 = ebx;          
 ebx = ebx + 0xacf6925;       
 ecx = local_u32_8;           
 ecx = ecx * ebx;          
 local_u32_13 = ecx;          
 state[14] = ecx;            
 ebx = edi;               
 ecx = 0x13;              
 ebx = ROR(ebx, ecx);          
 ecx = state[15];            
 ebx = ebx ^ ecx;          
 local_u32_14 = ebx;          
 state[15] = ebx;            
 esi = 0xc0c8f110;           
 esi = esi - ebx;          
 state[0] = esi;            
 eax = edi;               
 eax = eax | 0x2a57ebeb;       
 edi = edi ^ eax;          
 eax = edi;               
 ecx = 0xd;               
 eax = ROL(eax, ecx);          
 edx = local_u32_10;          
 edx = edx - eax;          
 state[12] = edx;            
 eax = local_u32_5;           
 eax = eax & 0x15a66bda;      
 edi = edi | eax;          
 ecx = local_u32_12;          
 edi = 0x235ac102 +  ecx + edi ; 
 eax = edi;               
 eax = eax + 0x2d;         
 ebx = local_u32_23;          
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 state[8] = ebx;            
 edx = edx + 0x3bbb70fe;      
 ebx = state[5];            
 edx = edx ^ ebx;          
 local_u32_15 = edx;          
 state[5] = edx;            
 eax = local_u32_26;          
 eax = eax + 0xec51134a;      
 edi = edi & eax;          
 ecx = local_u32_4 * 0x87095a6;   
 ebx = local_u32_24;          
 ecx = ecx ^ ebx;          
 local_u32_16 = ecx;          
 state[19] = ecx;            
 eax = edx;               
 eax = eax & 0xf43f6fb;       
 esi = esi - eax;          
 state[0] = esi;            
 ecx = local_u32_14;          
 ebx = local_u32_11;          
 ecx = 0xea66f8dc +  ebx + ecx ; 
 state[15] = ecx;            
 ecx = local_u32_16;          
 ecx = ecx + 0xd049cfd6;      
 ebx = local_u32_25;          
 ecx = ecx - ebx;          
 local_u32_17 = ecx;          
 state[19] = ecx;            
 eax = esi;               
 eax = eax ^ 0x253c86f9;      
 ecx = local_u32_5;           
 ecx = ecx - eax;          
 state[13] = ecx;            
 ebx = state[16];            
 local_u32_28 = ebx;          
 eax = ebx;               
 eax = eax | 0x520e84ba;       
 edi = edi + eax;          
 eax = local_u32_12;          
 ecx = 0x17;              
 eax = ROL(eax, ecx);          
 ebx = local_u32_9;           
 ebx = ebx - eax;          
 local_u32_18 = ebx;          
 state[10] = ebx;            
 eax = edi * 0x2ee5918a;      
 eax = eax + local_u32_12;     
 local_u32_19 = eax;          
 state[18] = eax;            
 eax = local_u32_4;           
 eax = eax ^ 0xf0a32bc;       
 eax = eax & local_u32_25;     
 state[7] = eax;            
 eax = edi;               
 eax = eax ^ 0xfa89177;       
 esi = esi - eax;          
 state[0] = esi;            
 eax = local_u32_7;           
 ecx = 0x18;              
 eax = ROL(eax, ecx);          
 eax = eax | local_u32_21;      
 state[2] = eax;            
 ecx = local_u32_13 * 0x1cb1574a;  
 state[14] = ecx;            
 ebx = edi;               
 ebx = ebx + 0x8d;         
 esi = ebx;               
 ebx = local_u32_18;          
 ecx = esi;               
 ebx = ROR(ebx, ecx);          
 local_u32_20 = ebx;          
 state[10] = ebx;            
 esi = local_u32_4;           
 esi = esi + 0xef291170;      
 esi = esi & local_u32_26;     
 state[1] = esi;            
 local_u32_17 = local_u32_17 & 0x259a6745; 
 ebx = local_u32_3;           
 ebx = ebx + local_u32_17;     
 state[6] = ebx;            
 eax = eax ^ 0x10467b8;       
 edi = edi - eax;          
 eax = local_u32_7;           
 eax = eax + 0xdbff9c2b;      
 eax = eax * local_u32_19;     
 state[18] = eax;            
 eax = edi;               
 eax = eax ^ 0x8d4c279;       
 eax = eax + local_u32_28;     
 state[16] = eax;            
 eax = local_u32_20;          
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 ecx = local_u32_15;          
 ecx = ecx - eax;          
 state[5] = ecx;            
 eax = edi;               
 return eax;
}


u32 mix_major20(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, edi, esi;
 u32 local_u32_0, local_u32_1, local_u32_2, local_u32_3;
 u32 local_u32_9, local_u32_10, local_u32_4, local_u32_11;
 u32 local_u32_12, local_u32_13, local_u32_6, local_u32_7;
 u32 local_u32_8, local_u32_5;
 eax = extra_state;           
 eax = eax & 0xce;         
 edx = state[14];            
 ecx = eax;               
 edx = ROL(edx, ecx);          
 local_u32_13 = edx;          
 state[14] = edx;            
 edx = state[15];            
 local_u32_9 = edx;           
 eax = edx;               
 eax = eax ^ 0xbf446ce;       
 extra_state = extra_state + eax;  
 ecx = extra_state;           
 ecx = 0xe227ea76 +  ecx + ecx ; 
 extra_state = ecx;           
 eax = state[19] * 0x50ee813;    
 eax = eax * local_u32_9;      
 state[19] = eax;            
 ebx = state[10];            
 ebx = ebx + 0x9e;         
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 edi = eax;               
 state[19] = edi;            
 eax = extra_state;           
 eax = eax & 0x372035b;       
 ecx = state[16];            
 ecx = ecx - eax;          
 local_u32_0 = ecx;           
 state[16] = ecx;            
 ebx = state[5];            
 edx = 0xd9d1da08 + ebx;      
 ecx = state[18];            
 edx = edx | ecx;          
 local_u32_1 = edx;           
 state[18] = edx;            
 edx = state[11];            
 local_u32_2 = edx;           
 eax = edx;               
 ecx = 0x9;               
 eax = ROL(eax, ecx);          
 ebx = ebx + eax;          
 local_u32_3 = ebx;           
 state[5] = ebx;            
 ecx = state[0];            
 local_u32_10 = ecx;          
 eax = ecx * 0xffffffc3;      
 ecx = edx;               
 edx = state[8];            
 ecx = eax;               
 edx = ROR(edx, ecx);          
 local_u32_4 = edx;           
 state[8] = edx;            
 eax = local_u32_10;          
 eax = eax ^ 0x46d0b40;       
 extra_state = extra_state + eax;  
 edx = state[1];            
 local_u32_11 = edx;          
 ebx = edx;               
 ebx = ebx + 0xe8684fc;       
 ebx = ebx ^ local_u32_13;     
 state[14] = ebx;            
 eax = ebx * 0x28f80128;      
 extra_state = extra_state - eax;  
 eax = edx;               
 ecx = 0xa;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state ^ eax;  
 edx = local_u32_0;           
 edx = edx + 0xa0397f;       
 edi = edi * edx;          
 local_u32_13 = edi;          
 state[19] = edi;            
 edx = extra_state;           
 ecx = 0x11;              
 edx = ROL(edx, ecx);          
 esi = edx;               
 esi = esi + state[13];       
 state[13] = esi;            
 edx = state[4];            
 edx = edx + 0x95670090;      
 ecx = state[6];            
 local_u32_12 = ecx;          
 edx = edx - ecx;          
 local_u32_5 = edx;           
 state[4] = edx;            
 extra_state = 0xf2eafbc6;       
 eax = extra_state;           
 eax = eax + 0x32;         
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 local_u32_6 = ebx;           
 state[14] = ebx;            
 edi = extra_state;           
 edi = edi + 0xf3369e63;      
 edi = edi * local_u32_10;     
 state[0] = edi;            
 eax = state[3];            
 eax = eax + 0xfa61efff;      
 eax = eax + local_u32_2;      
 local_u32_7 = eax;           
 state[3] = eax;            
 eax = state[12];            
 eax = eax ^ 0x75;         
 edx = extra_state;           
 ecx = eax;               
 edx = ROL(edx, ecx);          
 eax = local_u32_4;           
 eax = eax + 0xda64c153;      
 eax = eax - state[10];       
 state[8] = eax;            
 ebx = state[9];            
 ebx = ebx + 0x22a4da90;      
 ebx = ebx - eax;          
 state[9] = ebx;            
 eax = local_u32_13;          
 eax = eax | 0x2cd48d0d;       
 edx = edx ^ eax;          
 ecx = local_u32_13;          
 edx = 0xc6a5343a +  ecx + edx ; 
 extra_state = edx;           
 eax = 0xc3172899 + esi;      
 eax = eax - edx;          
 state[13] = eax;            
 edx = local_u32_5;           
 ecx = 0xb;               
 edx = ROL(edx, ecx);          
 ecx = local_u32_6;           
 edx = edx ^ ecx;          
 local_u32_8 = edx;           
 state[14] = edx;            
 edx = local_u32_13;          
 edx = edx ^ 0x274bf2e7;      
 ecx = local_u32_8;           
 edx = edx ^ ecx;          
 state[19] = edx;            
 ebx = ebx ^ 0x1448b87d;      
 ebx = ebx + local_u32_0;      
 state[16] = ebx;            
 edx = edx ^ 0x7c5e8091;      
 extra_state = extra_state & edx;  
 edx = local_u32_9;           
 edx = edx + 0x2de973cc;      
 ecx = extra_state;           
 edx = edx - ecx;          
 local_u32_13 = edx;          
 state[15] = edx;            
 edx = extra_state;           
 ecx = local_u32_3;           
 edx = 0xd0a90eaf +  ecx + edx ; 
 extra_state = edx;           
 edi = edi + 0x5cd4018;       
 edi = edi * state[17];       
 state[17] = edi;            
 eax = extra_state;           
 ecx = 0x6;               
 eax = ROR(eax, ecx);          
 ebx = extra_state;           
 ebx = ebx - eax;          
 eax = state[7];            
 eax = eax ^ 0x1c718ec4;      
 eax = eax * local_u32_13;     
 state[15] = eax;            
 eax = local_u32_8;           
 ecx = 0x1f;              
 eax = ROL(eax, ecx);          
 edx = local_u32_5;           
 ecx = eax;               
 edx = ROR(edx, ecx);          
 state[4] = edx;            
 eax = local_u32_11 * 0x11e6e4aa;  
 edx = local_u32_12;          
 edx = edx - eax;          
 state[6] = edx;            
 eax = local_u32_7;           
 eax = eax + 0x20a45ef;       
 eax = eax - local_u32_8;      
 state[3] = eax;            
 local_u32_2 = local_u32_2 ^ 0x79362e5; 
 edx = local_u32_8;           
 edx = edx & local_u32_2;      
 state[14] = edx;            
 ebx = ebx ^ 0x434171f9;      
 ebx = ebx ^ local_u32_1;      
 eax = ebx;               
 return eax;
}


u32 mix_major21(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_2;
 eax = state[11];            
 eax = eax ^ state[2];       
 edx = state[15];            
 eax = eax ^ edx;          
 local_u32_1 = eax;           
 esi = local_u32_1 % 0x0B;
 ebx = extra_state;           
 ebx = ebx | 0xde;          
 eax = state[13];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[13] = eax;            
 extra_state = extra_state + 0x67e07c3f; 
 ebx = state[6];            
 extra_state = extra_state - ebx;  
 edx = extra_state * 0x157052aa;  
 extra_state = extra_state ^ edx;  

 if (1 == esi) {            
  ebx = 0x30da4335;          
  ebx = ebx + 0xd72b9a3f;     
  eax = call(ebx, state, 0x6c3800da); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb736ba1;      
  eax = call(ebx, state, 0xc7ba581f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x235db53e;     
  eax = call(ebx, state, 0xb44b4b69); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1e954851;     
  ecx = extra_state;          
  eax = call(ebx, state, ecx);     
  ebx = state[6];           
 }

 eax = ebx;               
 ecx = 0xb;               
 eax = ROR(eax, ecx);          
 ecx = eax;               
 ebx = ROL(ebx, ecx);          
 state[6] = ebx;            
 eax = extra_state * 0x2437b7c7;  
 state[19] = state[19] + eax;    

 if (6 == esi) {            
  ebx = 0x4434e50b;          
  eax = 0xc3d195bd + ebx;     
  eax = call(eax, state, 0x5247491);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x264dde6d;     
  eax = call(ebx, state, 0xe8e4c45);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x11cc4375;     
  eax = call(ebx, state, 0xe6e8c2b7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x34539713;     
  eax = call(ebx, state, state[2]);  
 }

 eax = state[3];            
 eax = eax + 0xf9430940;      
 eax = eax + state[12];       
 state[3] = eax;            
 eax = state[6];            
 ecx = ecx ^ ecx;          
 eax = ROR(eax, ecx);          
 state[11] = state[11] - eax;    

 if (5 == esi) {            
  ebx = 0x86136c8;           
  eax = 0xffa6c7a4 + ebx;     
  eax = call(eax, state, 0xc58218a4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x70185c11;     
  eax = call(ebx, state, 0x339cbdf5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2ed96606;     
  eax = call(ebx, state, 0x806160db); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa0fd5b6;      
  eax = call(ebx, state, state[8]);  
 }

 eax = state[14];            
 eax = eax | 0x27c78ea;       
 state[0] = state[0] + eax;     
 ebx = extra_state;           
 ebx = ebx & 0x6b2cc678;      
 state[18] = state[18] - ebx;    
 ebx = state[11];            
 ebx = ebx & 0xca;         
 eax = state[15];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[15] = eax;            

 if (4 == esi) {            
  ebx = 0x6435daf9;          
  ebx = ebx + 0xa3d296ab;     
  eax = call(ebx, state, 0xd85fa165); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x356fdb00;     
  eax = call(ebx, state, 0xf6b8e3c7); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x42376dfb;     
  eax = call(ebx, state, 0xb66f3f3f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5f5707b6;     
  edx = extra_state;          
  eax = call(ebx, state, edx);     
 }

 eax = state[18];            
 ecx = 0x2;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state ^ eax;  
 eax = state[15] * 0x42515298;   
 state[10] = state[10] + eax;    
 eax = state[2];            
 eax = eax ^ 0x2a15668a;      
 state[19] = state[19] + eax;    

 if (7 == esi) {            
  ebx = 0x47b99cfb;          
  eax = 0xc04c709d + ebx;     
  eax = call(eax, state, 0xdb66bc4d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2a451bb8;     
  eax = call(ebx, state, 0xfe02c0e6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c400172;     
  eax = call(ebx, state, 0xcb1f30eb); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1058fe01;     
  eax = call(ebx, state, state[11]);  
 }

 eax = state[6];            
 eax = eax + 0xe28d6e07;      
 eax = eax - extra_state;      
 state[6] = eax;            
 eax = state[3];            
 eax = eax + 0x8a7848d;       
 state[1] = state[1] & eax;     
 eax = state[17];            
 eax = eax + 0xf76061aa;      
 eax = eax * state[10];       
 state[10] = eax;            

 if (0 == esi) {            
  ebx = 0x1a097e13;          
  eax = 0xedfd45a5 + ebx;     
  eax = call(eax, state, 0x36a66c96); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x272f25b9;     
  eax = call(ebx, state, 0x209ec3db); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4746d883;     
  eax = call(ebx, state, 0x636f8afc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b728cd9;     
  eax = call(ebx, state, state[12]);  
 }

 eax = state[1];            
 edx = eax;               
 ecx = 0x8;               
 edx = ROR(edx, ecx);          
 state[6] = state[6] + edx;     
 edx = state[2];            
 edx = edx | 0x16a41bdf;       
 eax = eax * edx;          
 state[1] = eax;            

 if (8 == esi) {            
  ebx = 0x4ef44d3d;          
  eax = 0xb91485f3 + ebx;     
  eax = call(eax, state, 0xa8e9c2c);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x37f92cc5;     
  eax = call(ebx, state, 0xb7817f0f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x72eb1b00;     
  eax = call(ebx, state, 0x5a1bbab6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2c9bd847;     
  eax = call(ebx, state, state[3]);  
 }

 eax = state[0];            
 eax = eax + 0x21889c31;      
 eax = eax - state[4];       
 state[0] = eax;            
 eax = extra_state;           
 eax = eax ^ 0x14a9f943;      
 eax = eax * extra_state;      
 extra_state = eax;           
 eax = state[13];            
 eax = eax + 0x5c58f04e;      
 state[5] = state[5] | eax;     
 eax = state[14];            
 eax = eax + 0x49437c23;      
 state[19] = state[19] ^ eax;    

 if (2 == esi) {            
  ebx = 0x45f9b56e;          
  eax = 0xc20c2806 + ebx;     
  eax = call(eax, state, 0xc952035f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xdf8cd43;      
  eax = call(ebx, state, 0x2105a21e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x434aa6e6;     
  eax = call(ebx, state, 0x10d28c9e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x96396a5;      
  eax = call(ebx, state, state[13]);  
 }

 eax = state[6];            
 eax = eax ^ 0x360a1ff0;      
 state[9] = state[9] | eax;     
 edx = state[14];            
 local_u32_0 = edx;           
 local_u32_0 = local_u32_0 << 7;    
 ecx = local_u32_0;           
 ecx = ecx + edx;          
 ecx = ecx << 11;            
 ecx = ecx + edx;          
 eax = edx + 4 * ecx;          
 eax = eax << 5;            
 eax = eax - edx;          
 eax = eax << 2;            
 eax = eax - edx;          
 state[13] = state[13] & eax;    
 edx = extra_state;           
 edx = 0x3053624 +  edx + edx ;  
 extra_state = edx;           

 if (3 == esi) {            
  ebx = 0x13ae07cd;          
  eax = 0xf45743fb + ebx;     
  eax = call(eax, state, 0x9fa8d97f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x300f02d5;     
  eax = call(ebx, state, 0x86c39014); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1fd3fc65;     
  eax = call(ebx, state, 0x8c273c75); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x704727fa;     
  eax = call(ebx, state, state[17]);  
 }

 extra_state = extra_state + 0xc7af02f5; 
 ecx = state[1];            
 extra_state = extra_state - ecx;  
 extra_state = extra_state & 0xc11a9b11; 
 eax = state[12];            
 eax = eax + 0xac2e6058;      
 state[6] = state[6] ^ eax;     
 edx = state[17];            
 edx = edx + 0xd87e9f50;      
 ecx = state[12];            
 ecx = ecx ^ edx;          
 state[12] = ecx;            

 if (10 == esi) {            
  ebx = 0x257a3cd0;          
  eax = 0xe28d9b28 + ebx;     
  eax = call(eax, state, 0xc7b49f8);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb261fb9;      
  eax = call(ebx, state, 0x131c4743); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xca94180;      
  eax = call(ebx, state, 0x310e85ab); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa485a1e;      
  eax = call(ebx, state, state[6]);  
 }

 ebx = state[7];            
 ebx = ebx ^ 0xe1;         
 eax = state[9];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[9] = eax;            
 edx = state[14];            
 edx = edx ^ 0xff63c7c;       
 extra_state = extra_state + edx;  

 if (9 == esi) {            
  ebx = 0x30a182ae;          
  eax = 0xd763f192 + ebx;     
  eax = call(eax, state, 0x4a7d7f69); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb164938;      
  eax = call(ebx, state, 0x639b2836); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x32d0a1f8;     
  eax = call(ebx, state, 0xf816391f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x96116fa;      
  eax = call(ebx, state, state[5]);  
 }

 esi = state[9];            
 esi = esi ^ 0x132ee304;      
 ebx = state[6];            
 esi = esi ^ ebx;          
 state[9] = esi;            
 ecx = state[14];            
 ecx = ecx + 0x11e0a175;      
 ecx = ecx * state[12];       
 state[12] = ecx;            
 eax = extra_state;           
 local_u32_1 = eax;           
 local_u32_1 = local_u32_1 ^ 0x267e2568; 
 edx = state[14];            
 edx = edx - local_u32_1;      
 local_u32_2 = edx;           
 state[14] = edx;            
 eax = state[3];            
 ecx = 0xb;               
 eax = ROL(eax, ecx);          
 local_u32_1 = eax;           
 eax = state[0];            
 ecx = local_u32_1;           
 eax = ROL(eax, ecx);          
 state[0] = eax;            
 edx = state[8];            
 edx = edx ^ 0xe173238;       
 edx = edx ^ ebx;          
 state[8] = edx;            
 ebx = ebx + 0xee9e5b6a;      
 ebx = ebx * eax;          
 state[0] = ebx;            
 ebx = state[15];            
 ecx = ebx * 0x1fe0f470;      
 esi = esi | ecx;          
 state[9] = esi;            
 eax = state[2];            
 eax = eax + 0x69;         
 esi = eax;               
 eax = state[2];            
 ecx = esi;               
 eax = ROL(eax, ecx);          
 state[2] = eax;            
 edx = local_u32_2 * 0x1b4bf87b;  
 ecx = state[16];            
 edx = edx ^ ecx;          
 state[16] = edx;            
 eax = state[10];            
 eax = eax + 0x2383020a;      
 eax = eax & edx;          
 local_u32_1 = eax;           
 state[16] = eax;            
 edx = state[7];            
 ebx = 0xeb32d6f9 +  edx + ebx ; 
 ecx = 0x11;              
 eax = ROL(eax, ecx);          
 ebx = ebx ^ eax;          
 state[15] = ebx;            
 ebx = extra_state;           
 ebx = ebx | 0x20914367;       
 ebx = ebx + local_u32_1;      
 state[16] = ebx;            
 eax = extra_state;           
 return eax;
}


u32 mix_major22(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_2;
 ebx = extra_state;           
 esi = ebx % 0x0B;
 ebx = ebx ^ 0xc3115e;       
 state[12] = state[12] + ebx;    
 eax = state[19];            
 eax = eax + 0x4f9d3712;      
 eax = eax - extra_state;      
 state[19] = eax;            
 edx = state[11] * 0x37e68d12;   
 state[16] = state[16] & edx;    

 if (1 == esi) {            
  ebx = 0x702cb8fb;          
  eax = 0x97da06b5 + ebx;     
  eax = call(eax, state, 0x1e03d70a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x419fc1e3;     
  eax = call(ebx, state, 0x8b9fc88e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2688408f;     
  eax = call(ebx, state, 0xa966b21d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4a24c19f;     
  eax = call(ebx, state, state[7]);  
 }

 edx = state[18];            
 eax = edx;               
 eax = eax ^ 0x4ea934da;      
 extra_state = extra_state - eax;  
 edx = edx ^ 0x18a1ba1a;      
 state[1] = state[1] & edx;     

 if (0 == esi) {            
  ebx = 0x30e0a1fa;          
  ebx = ebx + 0xd72946f2;     
  eax = call(ebx, state, 0x2f3d2bc4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x10fff3bd;     
  eax = call(ebx, state, 0x36d5c53e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2b85d7e9;     
  eax = call(ebx, state, 0x963dc5d);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x263f9a3e;     
  ecx = extra_state;          
  eax = call(ebx, state, ecx);     
 }

 eax = state[17] * 0x3bf23dc7;   
 state[18] = state[18] + eax;    
 edx = state[5];            
 eax = edx;               
 eax = eax ^ 0x3537eae2;      
 state[12] = state[12] + eax;    
 eax = state[9];            
 eax = eax + 0xf4d4e1ee;      
 eax = eax + edx;          
 state[9] = eax;            
 eax = state[16];            
 ecx = 0x16;              
 eax = ROL(eax, ecx);          
 state[11] = state[11] - eax;    

 if (2 == esi) {            
  ebx = 0x345d544c;          
  eax = 0xd3ab1d58 + ebx;     
  eax = call(eax, state, 0x400fb62);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x99351ad;      
  eax = call(ebx, state, 0xcaaec0f4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x42dffe77;     
  eax = call(ebx, state, 0xfdc34e52); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a5d42d3;     
  eax = call(ebx, state, state[16]);  
 }

 ebx = state[1];            
 ebx = ebx ^ 0x80;         
 eax = state[17];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[17] = eax;            
 eax = state[5];            
 ecx = 0x1;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state | eax;  
 edx = state[11];            
 edx = edx + 0xf0871714;      
 edx = edx + extra_state;      
 state[11] = edx;            

 if (3 == esi) {            
  ebx = 0x33cf7c0e;          
  eax = 0xd4380352 + ebx;     
  eax = call(eax, state, 0xed238c8b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3be0f255;     
  eax = call(ebx, state, 0xa7549ca);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x47ee842e;     
  eax = call(ebx, state, 0x1b430ac9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6fe170a9;     
  eax = call(ebx, state, state[18]);  
 }

 extra_state = extra_state & 0x1b54f10; 
 eax = state[15];            
 eax = eax + 0xe9b29695;      
 eax = eax + state[1];       
 state[15] = eax;            
 eax = state[19];            
 eax = eax + 0xf9850900;      
 state[9] = state[9] ^ eax;     

 if (8 == esi) {            
  ebx = 0x39c157ce;          
  eax = 0xce4567e2 + ebx;     
  eax = call(eax, state, 0x2b0ca8e6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x329166ef;     
  eax = call(ebx, state, 0xecb73833); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xfdc70ea;      
  eax = call(ebx, state, 0x7e9a5820); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4c30e61d;     
  eax = call(ebx, state, state[11]);  
 }

 eax = state[0];            
 eax = eax + 0x224785;       
 eax = eax + state[6];       
 state[0] = eax;            
 eax = state[9] * 0x602a9ff;    
 state[1] = state[1] - eax;     

 if (4 == esi) {            
  ebx = 0x3607eb1c;          
  eax = 0xd201f7dc + ebx;     
  eax = call(eax, state, 0xcd96ea47); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x55cf4a09;     
  eax = call(ebx, state, 0x2975b593); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x16c5a830;     
  eax = call(ebx, state, 0x474baee1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5b0b3b81;     
  eax = call(ebx, state, state[10]);  
 }

 eax = state[5];            
 ecx = 0x1e;              
 eax = ROL(eax, ecx);          
 state[14] = state[14] + eax;    
 eax = state[12] * 0x223c8eff;   
 state[8] = state[8] - eax;     
 eax = state[11] * 0xc99e9b5;    
 state[3] = state[3] + eax;     

 if (7 == esi) {            
  ebx = 0x681bd9ff;          
  eax = 0x9fec2495 + ebx;     
  eax = call(eax, state, 0x7fb35893); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1aacf9b4;     
  eax = call(ebx, state, 0x30afb834); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1d1b6cad;     
  eax = call(ebx, state, 0x3facef1);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xea6f41d;      
  eax = call(ebx, state, state[13]);  
 }

 eax = state[3];            
 eax = eax ^ 0xf8e252d;       
 eax = eax * extra_state;      
 extra_state = eax;           
 eax = eax & 0xa58c765;       
 state[12] = state[12] + eax;    

 if (10 == esi) {            
  ebx = 0x439d443e;          
  eax = 0xc46a9a02 + ebx;     
  eax = call(eax, state, 0x9eafe104); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f361f94;     
  eax = call(ebx, state, 0x4e98d8f0); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xeb18693;      
  eax = call(ebx, state, 0x74a127c5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4267cf74;     
  eax = call(ebx, state, state[13]);  
 }

 eax = state[3];            
 eax = eax ^ 0x59507436;      
 state[11] = state[11] - eax;    
 eax = state[10];            
 eax = eax ^ 0x1082cbd7;      
 eax = eax ^ extra_state;      
 state[10] = eax;            

 if (9 == esi) {            
  ebx = 0x3a0a5eaf;          
  eax = 0xcdfbaf11 + ebx;     
  eax = call(eax, state, 0xd9aa991c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a0d82d0;     
  eax = call(ebx, state, 0x7d828c2b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x157aa85b;     
  eax = call(ebx, state, 0x3430b0d1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b6b7b1e;     
  eax = call(ebx, state, state[3]);  
 }

 ebx = state[2] * 0x22b21be0;    
 eax = state[3];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[3] = eax;            
 ebx = extra_state;           
 ebx = ebx + 0xc1;         
 eax = state[8];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[8] = eax;            
 edx = state[15] * 0x1627a9f4;   
 state[17] = state[17] ^ edx;    

 if (6 == esi) {            
  ebx = 0x2a676f96;          
  ebx = ebx + 0xdd9e6e02;     
  eax = call(ebx, state, 0x7725a142); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x456f03c3;     
  eax = call(ebx, state, 0x3c2476da); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x39f235f3;     
  eax = call(ebx, state, 0x11c27e33); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x112d35d0;     
  ecx = extra_state;          
  eax = call(ebx, state, ecx);     
 }

 eax = state[11];            
 edx = eax;               
 ecx = 0x15;              
 edx = ROL(edx, ecx);          
 edx = edx * extra_state;      
 extra_state = edx;           
 eax = eax + 0x27d2e810;      
 eax = eax ^ state[3];       
 state[3] = eax;            
 edx = state[16] * 0x2bb9259f;   
 eax = eax + edx;          
 state[3] = eax;            

 if (5 == esi) {            
  ebx = 0x5b1d95ac;          
  ebx = ebx + 0xacea68c0;     
  eax = call(ebx, state, 0x7f4eb374); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4cfcfb25;     
  eax = call(ebx, state, 0x88ae07f);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2e7983f6;     
  eax = call(ebx, state, 0x62b4120a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x30530b43;     
  eax = extra_state;          
  eax = call(ebx, state, eax);     
 }

 edx = state[19];            
 edx = edx ^ 0x2b7f6e80;      
 ecx = state[17];            
 edx = edx ^ ecx;          
 local_u32_1 = edx;           
 state[19] = edx;            
 ebx = state[0];            
 ecx = 0x18;              
 ebx = ROL(ebx, ecx);          
 ebx = ebx ^ state[7];       
 state[7] = ebx;            
 esi = extra_state;           
 esi = esi | 0x334e9536;       
 esi = esi ^ state[4];       
 state[4] = esi;            
 eax = extra_state;           
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 edx = state[11];            
 edx = edx - eax;          
 state[11] = edx;            
 ecx = extra_state;           
 ecx = ecx + 0xf8e5b64c;      
 eax = state[12];            
 ecx = ecx ^ eax;          
 local_u32_2 = ecx;           
 state[12] = ecx;            
 esi = 0x661bc871 +  esi + edx ; 
 state[11] = esi;            
 edx = state[0];            
 edx = edx & 0xc9;         
 eax = local_u32_1;           
 ecx = edx;               
 eax = ROR(eax, ecx);          
 local_u32_1 = eax;           
 state[19] = eax;            
 edx = extra_state;           
 local_u32_0 = edx;           
 local_u32_0 = local_u32_0 & 0x7b85306; 
 ecx = local_u32_0;           
 state[15] = state[15] + ecx;    
 ebx = ebx + 0x1394a239;      
 ebx = ebx - local_u32_2;      
 state[7] = ebx;            
 eax = state[3];            
 eax = eax + 0x4d2d2d3c;      
 eax = eax ^ state[17];       
 state[17] = eax;            
 ebx = state[6];            
 local_u32_0 = ebx;           
 local_u32_0 = local_u32_0 & 0x312a10; 
 edx = local_u32_2;           
 edx = edx - local_u32_0;      
 local_u32_2 = edx;           
 state[12] = edx;            
 ecx = local_u32_1;           
 ecx = ecx + 0xba345c89;      
 eax = state[13];            
 ecx = ecx | eax;          
 edx = extra_state;           
 edx = edx + 0x2098c7b4;      
 ecx = ecx * edx;          
 state[13] = ecx;            
 eax = state[10];            
 ecx = 0x16;              
 eax = ROL(eax, ecx);          
 ebx = ebx & eax;          
 local_u32_2 = local_u32_2 & 0x13175e3d; 
 ebx = ebx - local_u32_2;      
 state[6] = ebx;            
 eax = extra_state;           
 return eax;
}


u32 mix_major23(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_1, local_u32_3;
 u8 local_u8_17;
 ebx = extra_state;           
 esi = ebx % 0x0B;
 eax = state[5];            
 ecx = 0xb;               
 eax = ROL(eax, ecx);          
 extra_state = extra_state & eax;  
 eax = extra_state;           
 ecx = 0x17;              
 eax = ROR(eax, ecx);          
 state[18] = state[18] - eax;    
 edx = state[19];            
 edx = edx + 0xb42a2f00;      
 edx = edx + extra_state;      
 state[19] = edx;            

 if (5 == esi) {            
  ebx = 0xa6cd589;           
  eax = 0xfd99ea7f + ebx;     
  eax = call(eax, state, 0x3299ed82); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x152bf5ea;     
  eax = call(ebx, state, 0x5fb25c93); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa6e607b;      
  eax = call(ebx, state, 0x656b56c1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x702c0b9a;     
  eax = call(ebx, state, state[2]);  
 }

 ecx = state[0];            
 ecx = ecx + 0x71507fd7;      
 ecx = ecx + state[12];       
 state[0] = ecx;            
 eax = extra_state;           
 edx = state[19];            
 eax = 0x9a68096 +  edx + eax ;  
 extra_state = eax;           

 if (6 == esi) {            
  ebx = 0xdac9858;           
  eax = 0xfa5ae6e4 + ebx;     
  eax = call(eax, state, 0x322e0cfc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2057f051;     
  eax = call(ebx, state, 0xa6cc505f); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1efe293f;     
  eax = call(ebx, state, 0xf24ba18a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x394280dd;     
  eax = call(ebx, state, state[10]);  
 }

 edx = state[0];            
 edx = edx + 0x238788d8;      
 edx = edx + state[2];       
 state[0] = edx;            
 ebx = state[15];            
 ebx = ebx + 0xd6;         
 eax = state[3];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[3] = eax;            
 edx = state[10];            
 edx = edx + 0xdf1e2fab;      
 edx = edx - state[9];       
 state[10] = edx;            

 if (3 == esi) {            
  ebx = 0xe4a75d2;           
  ebx = ebx + 0xf9bb97ee;     
  eax = call(ebx, state, 0xec209445); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8905b5f;      
  eax = call(ebx, state, 0x3d280587); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f4ee89b;     
  eax = call(ebx, state, 0xb5128462); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4b7a3a86;     
  ecx = extra_state;          
  eax = call(ebx, state, ecx);     
 }

 eax = extra_state;           
 ecx = 0x1b;              
 eax = ROL(eax, ecx);          
 extra_state = extra_state ^ eax;  
 eax = state[11];            
 ecx = 0x17;              
 eax = ROR(eax, ecx);          
 extra_state = extra_state - eax;  

 if (7 == esi) {            
  ebx = 0x20544474;          
  eax = 0xe7b5a4c4 + ebx;     
  eax = call(eax, state, 0xcd2a804a); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2ccece0e;     
  eax = call(ebx, state, 0xcf5e15d2); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa1a4542;      
  eax = call(ebx, state, 0xadfc7bd);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x17e64416;     
  eax = call(ebx, state, state[15]);  
 }

 eax = state[18];            
 eax = eax + 0x13ba6066;      
 edx = state[10];            
 eax = eax + edx;          
 state[18] = eax;            
 eax = state[11];            
 eax = eax + 0xd44a337d;      
 eax = eax - edx;          
 state[11] = eax;            
 eax = state[3];            
 eax = eax + 0xad722336;      
 state[17] = state[17] & eax;    

 if (4 == esi) {            
  ebx = 0xb6e39a5;           
  eax = 0xfc99171b + ebx;     
  eax = call(eax, state, 0x46a58b1);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2864e382;     
  eax = call(ebx, state, 0xa734eeb5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x74416108;     
  eax = call(ebx, state, 0x2fa0a3e5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xc557dad;      
  eax = call(ebx, state, state[8]);  
 }

 eax = extra_state;           
 ecx = 0xf;               
 eax = ROL(eax, ecx);          
 edx = state[7];            
 edx = edx - eax;          
 state[7] = edx;            
 eax = state[6];            
 eax = eax + 0x45d2e311;      
 extra_state = extra_state | eax;  
 eax = 0xd196f18f + edx;      
 extra_state = extra_state ^ eax;  
 eax = extra_state;           
 eax = eax ^ 0x48;         
 ecx = eax;               
 edx = ROL(edx, ecx);          
 state[7] = edx;            

 if (8 == esi) {            
  ebx = 0x1a63770a;          
  eax = 0xeda6720a + ebx;     
  eax = call(eax, state, 0x6b73b12);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12c598f5;     
  eax = call(ebx, state, 0x96165b93); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x7c601946;     
  eax = call(ebx, state, 0x76fb5502); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3a38ace7;     
  eax = call(ebx, state, state[13]);  
 }

 eax = state[18] * 0x413db8c1;   
 state[6] = state[6] + eax;     
 eax = state[19];            
 eax = eax + 0x2be41642;      
 state[0] = state[0] ^ eax;     
 eax = state[9];            
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 eax = eax * state[4];       
 state[4] = eax;            

 if (10 == esi) {            
  ebx = 0xe88e3f4;           
  ebx = ebx + 0xf97db100;     
  eax = call(ebx, state, 0x76fc0fc3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x621f7db0;     
  eax = call(ebx, state, 0x99abb31b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2abc77f5;     
  eax = call(ebx, state, 0xecd9600b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13e31049;     
  eax = extra_state;          
  eax = call(ebx, state, eax);     
 }

 ebx = state[6] * 0x20d47013;    
 eax = extra_state;           
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 extra_state = eax;           
 edx = state[3];            
 edx = edx & 0x9262077;       
 edx = edx * state[17];       
 state[17] = edx;            
 ecx = state[14];            
 ecx = ecx + 0xfa8ae5a0;      
 state[13] = state[13] ^ ecx;    

 if (1 == esi) {            
  ebx = 0x12fabde5;          
  eax = 0xf50b303f + ebx;     
  eax = call(eax, state, 0xcf2aff70); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5ac4b739;     
  eax = call(ebx, state, 0xd8ed9a57); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3dcf30b6;     
  eax = call(ebx, state, 0x3c340720); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x99773f6;      
  eax = call(ebx, state, state[13]);  
 }

 ebx = state[2];            
 ecx = 0x11;              
 ebx = ROL(ebx, ecx);          
 eax = extra_state;           
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 extra_state = eax;           
 edx = state[13];            
 edx = edx + 0xffd58fe8;      
 edx = edx - state[8];       
 state[13] = edx;            
 ecx = state[6];            
 ecx = ecx ^ 0x1d606322;      
 eax = state[8];            
 eax = eax + ecx;          
 state[8] = eax;            

 if (9 == esi) {            
  ebx = 0x1de7e6a7;          
  eax = 0xea1e9401 + ebx;     
  eax = call(eax, state, 0x69ef8893); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x4a0d98ab;     
  eax = call(ebx, state, 0xe33b026c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3f7323c8;     
  eax = call(ebx, state, 0x26ca48f6); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa0816bf;      
  eax = call(ebx, state, state[10]);  
 }

 eax = state[16];            
 eax = eax + 0xe3a240f7;      
 eax = eax + state[19];       
 state[16] = eax;            
 eax = state[14];            
 ecx = 0x3;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state ^ eax;  

 if (0 == esi) {            
  ebx = 0x25ef82dd;          
  eax = 0xe218efc3 + ebx;     
  eax = call(eax, state, 0x4cb39f95); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x942a559;      
  eax = call(ebx, state, 0xc2043573); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x334fec60;     
  eax = call(ebx, state, 0x9d6390ca); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x6baef283;     
  eax = call(ebx, state, state[4]);  
 }

 eax = state[7];            
 eax = eax | 0x196e1a4c;       
 eax = eax ^ state[18];       
 state[18] = eax;            
 eax = eax ^ 0xffcac8f;       
 extra_state = extra_state + eax;  
 eax = state[1];            
 eax = eax ^ 0xb09adec;       
 eax = eax ^ state[0];       
 state[1] = eax;            

 if (2 == esi) {            
  ebx = 0x26633a6b;          
  eax = 0xe1a6a88d + ebx;     
  eax = call(eax, state, 0x7849b1e);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3d8402b6;     
  eax = call(ebx, state, 0x1c2fd0b9); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x5f9918df;     
  eax = call(ebx, state, 0x4be7977);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8ce694a;      
  eax = call(ebx, state, state[11]);  
 }

 eax = state[2];            
 eax = eax + 0x328852b1;      
 eax = eax * state[14];       
 local_u32_0 = eax;           
 state[14] = eax;            
 edx = state[15];            
 edx = edx & 0x1e0a37a;       
 ecx = state[8];            
 edx = edx ^ ecx;          
 local_u32_3 = edx;           
 state[8] = edx;            
 eax = extra_state;           
 ecx = 0xd;               
 eax = ROL(eax, ecx);          
 esi = eax;               
 esi = esi * state[3];       
 state[3] = esi;            
 edx = state[18];            
 edx = edx + 0xc9c48b38;      
 state[6] = state[6] ^ edx;     
 ecx = state[14];            
 ecx = ecx + 0x7d;         
 local_u8_17 = ecx;           
 ebx = state[2];            
 ebx = ROL(ebx, ecx);          
 state[2] = ebx;            
 eax = state[10];            
 local_u32_1 = eax;           
 ecx = 0xd;               
 eax = ROR(eax, ecx);          
 extra_state = extra_state ^ eax;  
 local_u8_17 = local_u8_17 + 0x86; 
 eax = extra_state;           
 ecx = local_u8_17;           
 eax = ROR(eax, ecx);          
 edx = local_u32_3;           
 edx = edx + 0xef774f5b;      
 state[12] = state[12] ^ edx;    
 local_u32_3 = eax;           
 local_u32_3 = local_u32_3 ^ 0x58f00a07; 
 ecx = local_u32_3;           
 eax = eax + ecx;          
 extra_state = eax;           
 eax = state[9];            
 eax = eax ^ 0x5483deb2;      
 eax = eax ^ extra_state;      
 state[9] = eax;            
 edx = state[0] * 0x2c63f116;    
 edx = edx | local_u32_0;      
 state[14] = edx;            
 local_u32_1 = local_u32_1 ^ 0xa051af; 
 esi = esi + local_u32_1;      
 state[3] = esi;            
 extra_state = extra_state + 0xfdb247f0; 
 ecx = state[0];            
 extra_state = extra_state - ecx;  
 ebx = ebx + 0xf9432db1;      
 ebx = ebx - extra_state;      
 state[2] = ebx;            
 eax = extra_state;           
 return eax;
}


u32 mix_major24(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 u32 local_u32_0, local_u32_2, local_u32_3;
 eax = state[17];            
 eax = eax ^ state[8];       
 edx = state[10];            
 eax = eax ^ edx;          
 local_u32_2 = eax;           
 esi = local_u32_2 % 0x0B;
 eax = state[7];            
 ecx = ecx ^ ecx;          
 eax = ROL(eax, ecx);          
 eax = eax * extra_state;      
 eax = eax ^ 0x13a77c41;      
 edx = state[0];            
 eax = eax ^ edx;          
 local_u32_0 = eax;           
 ebx = state[3];            
 ebx = ebx + 0x10;         
 eax = state[2];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[2] = eax;            

 if (1 == esi) {            
  ebx = 0x1490cec4;          
  eax = 0xf375c650 + ebx;     
  eax = call(eax, state, 0x30ff7e08); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x488919be;     
  eax = call(ebx, state, 0xc7d3347e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28e9e772;     
  eax = call(ebx, state, 0x2de21bf8); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2693299b;     
  eax = call(ebx, state, state[12]);  
 }

 edx = local_u32_0;           
 edx = edx + 0xf4135aef;      
 local_u32_0 = local_u32_0 ^ edx;  
 ebx = state[6];            
 ebx = ebx + 0x9;          
 eax = local_u32_0;           
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 local_u32_0 = eax;           
 eax = state[13];            
 ecx = 0x19;              
 eax = ROL(eax, ecx);          
 state[14] = state[14] + eax;    
 edx = state[8];            
 edx = edx + 0x19454e81;      
 state[16] = state[16] ^ edx;    

 if (10 == esi) {            
  ebx = 0x18e724fc;          
  eax = 0xef1f9e9c + ebx;     
  eax = call(eax, state, 0xe80f6749); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xf9d18f2;      
  eax = call(ebx, state, 0xc1ae7aad); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x21073e69;     
  eax = call(ebx, state, 0x197cf7f5); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1309a52e;     
  eax = call(ebx, state, state[8]);  
 }

 ecx = local_u32_0;           
 ecx = ecx + 0xcb4ea17e;      
 ecx = ecx * state[3];       
 state[3] = ecx;            
 ebx = state[17];            
 ebx = ebx ^ 0x34;         
 eax = state[17];            
 ecx = ebx;               
 eax = ROL(eax, ecx);          
 state[17] = eax;            
 edx = state[11] * 0x2c0fd99b;   
 local_u32_0 = local_u32_0 - edx;  

 if (3 == esi) {            
  ebx = 0x288bfc73;          
  ebx = ebx + 0xdf7c0221;     
  eax = call(ebx, state, 0x85377b45); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x352edeae;     
  eax = call(ebx, state, 0x5027e457); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x15bb1318;     
  eax = call(ebx, state, 0xea83899c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x3ab49695;     
  ecx = local_u32_0;          
  eax = call(ebx, state, ecx);     
 }

 eax = state[12];            
 eax = eax + 0x7e55995;       
 eax = eax + state[19];       
 state[12] = eax;            
 eax = state[13] * 0x3dd1a491;   
 state[14] = state[14] - eax;    
 edx = state[8];            
 eax = edx;               
 eax = eax & 0x162b97ec;      
 state[4] = state[4] | eax;     
 edx = edx + state[3];       
 edx = edx + 0xc3000fb6;      
 state[8] = edx;            

 if (6 == esi) {            
  ebx = 0x19ffb15c;          
  eax = 0xee0a37dc + ebx;     
  eax = call(eax, state, 0xd4f24071); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2f41d1ce;     
  eax = call(ebx, state, 0x425840fc); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2007afe3;     
  eax = call(ebx, state, 0x2fd37052); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x805684f;      
  eax = call(ebx, state, state[9]);  
 }

 eax = state[8];            
 eax = eax ^ 0x2a161224;      
 state[13] = state[13] + eax;    
 eax = state[1] * 0xc693c6b;    
 eax = eax + state[10];       
 state[10] = eax;            
 eax = eax + 0xecde6b96;      
 eax = eax * state[4];       
 state[4] = eax;            

 if (9 == esi) {            
  ebx = 0x437b530e;          
  eax = 0xc48cab5e + ebx;     
  eax = call(eax, state, 0x18e7a164); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x19c96e35;     
  eax = call(ebx, state, 0x3f94809c); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xe08ac69;      
  eax = call(ebx, state, 0x66e81e44); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x40ccd00d;     
  eax = call(ebx, state, state[13]);  
 }

 eax = state[13];            
 ecx = 0x19;              
 eax = ROR(eax, ecx);          
 eax = eax * state[8];       
 state[8] = eax;            
 eax = state[14];            
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 state[17] = state[17] ^ eax;    
 eax = state[4];            
 eax = eax + 0x1c938114;      
 local_u32_0 = local_u32_0 & eax;  

 if (2 == esi) {            
  ebx = 0x28e39724;          
  ebx = ebx + 0xdf232908;     
  eax = call(ebx, state, 0xc053579b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x13d3c13d;     
  eax = call(ebx, state, 0x66f9a34e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1b28e67c;     
  eax = call(ebx, state, 0xeadbae6d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x24b8f828;     
  eax = local_u32_0;          
  eax = call(ebx, state, eax);     
 }

 eax = local_u32_0;           
 eax = eax + 0xc328858;       
 eax = eax * state[0];       
 state[0] = eax;            
 eax = state[15];            
 eax = eax | 0x137d6d8;       
 local_u32_0 = local_u32_0 + eax;  
 eax = state[3];            
 eax = eax + 0xae4f0ae;       
 eax = eax - state[9];       
 state[3] = eax;            

 if (0 == esi) {            
  ebx = 0x49545c5d;          
  eax = 0xbeb45e97 + ebx;     
  eax = call(eax, state, 0xa1fa0018); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x47aae46b;     
  eax = call(ebx, state, 0x1dd709e1); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2f3b330f;     
  eax = call(ebx, state, 0x4e3fd7db); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2639d4d4;     
  eax = call(ebx, state, state[3]);  
 }

 edx = state[10];            
 edx = edx + 0xe55615;       
 edx = edx * local_u32_0;      
 ecx = state[15];            
 ecx = ecx | 0x120d32e3;       
 ecx = ecx | edx;          
 state[15] = ecx;            
 ebx = state[15];            
 ebx = ebx ^ 0x2c;         
 ecx = ebx;               
 edx = ROL(edx, ecx);          
 local_u32_0 = edx;           
 ebx = state[7];            
 ecx = ecx ^ ecx;          
 ebx = ROR(ebx, ecx);          
 eax = state[6];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 state[6] = eax;            

 if (7 == esi) {            
  ebx = 0x8a8d7d7;           
  eax = 0xff5d05e9 + ebx;     
  eax = call(eax, state, 0xeacbc54);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb442a97;      
  eax = call(ebx, state, 0x6fbd10d4); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xaa7ef99;      
  eax = call(ebx, state, 0x39f736fe); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x12033c0d;     
  eax = call(ebx, state, state[17]);  
 }

 edx = state[4];            
 eax = edx;               
 eax = eax | 0x2587388f;       
 state[3] = state[3] - eax;     
 eax = state[2];            
 eax = eax + 0xffda87c9;      
 eax = eax + edx;          
 state[2] = eax;            
 ecx = 0x11;              
 eax = ROR(eax, ecx);          
 local_u32_0 = local_u32_0 - eax;  
 eax = state[6] * 0x34aabe3a;    
 state[1] = state[1] + eax;     

 if (4 == esi) {            
  ebx = 0x284694cb;          
  eax = 0xdfc02f35 + ebx;     
  eax = call(eax, state, 0xf7024f3e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x28b5a4f6;     
  eax = call(ebx, state, 0x3ba2eb20); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x2239847a;     
  eax = call(ebx, state, 0x6f94795b); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa8e3990;      
  eax = call(ebx, state, state[16]);  
 }

 eax = state[17];            
 eax = eax ^ 0x3d17e55a;      
 eax = eax ^ state[13];       
 state[17] = eax;            
 eax = state[14];            
 eax = eax + 0xdaf5121;       
 eax = eax * state[15];       
 state[15] = eax;            

 if (5 == esi) {            
  ebx = 0x1506bb07;          
  ebx = ebx + 0xf301438d;     
  eax = call(ebx, state, 0x7f5ebb7d); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1ac91686;     
  eax = call(ebx, state, 0x533af768); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xb263305;      
  eax = call(ebx, state, 0x473d981e); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x14cad1ef;     
  eax = local_u32_0;          
  eax = call(ebx, state, eax);     
 }

 ecx = state[17] * 0x1c8e3df4;   
 edx = state[6];            
 edx = ROL(edx, ecx);          
 state[6] = edx;            
 eax = state[15];            
 eax = eax ^ 0x14819516;      
 edx = edx + eax;          
 state[6] = edx;            

 if (8 == esi) {            
  ebx = 0x42dd23d6;          
  eax = 0xc52956f2 + ebx;     
  eax = call(eax, state, 0x463d99a3); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x1a270bbc;     
  eax = call(ebx, state, 0xec4bdf78); 
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0x8a91858;      
  eax = call(ebx, state, 0x2c290c4);  
  ebx = eax;              
  ebx = 0-ebx;             
  ebx = ebx + 0xa6261b3;      
  eax = call(ebx, state, state[5]);  
 }

 edx = state[14];            
 edx = edx + 0xc735f228;      
 state[8] = state[8] | edx;     
 ebx = state[17];            
 ebx = ebx + 0x7e;         
 eax = state[7];            
 ecx = ebx;               
 eax = ROR(eax, ecx);          
 esi = eax;               
 state[7] = esi;            
 edx = local_u32_0 * 0x340d3ff2;  
 edx = edx * state[10];       
 local_u32_0 = edx;           
 ecx = state[14];            
 ecx = ecx + 0x57a8d4b3;      
 ecx = ecx * state[16];       
 state[16] = ecx;            
 eax = state[6];            
 eax = eax + 0x534be48e;      
 eax = eax - state[1];       
 state[6] = eax;            
 ebx = state[9] * 0xd695251;    
 ebx = ebx ^ state[2];       
 state[2] = ebx;            
 eax = esi;               
 ecx = ecx ^ ecx;          
 eax = ROR(eax, ecx);          
 state[12] = state[12] ^ eax;    
 edx = state[17];            
 local_u32_3 = edx;           
 ecx = state[1];            
 ecx = 0xf022cb99 +  edx + ecx ; 
 state[1] = ecx;            
 eax = local_u32_0;           
 local_u32_2 = eax;           
 local_u32_2 = local_u32_2 | 0x2954ac20; 
 edx = local_u32_2;           
 state[4] = state[4] + edx;     
 local_u32_2 = eax;           
 local_u32_2 = local_u32_2 ^ 0x1b904466; 
 esi = esi * local_u32_2;      
 state[7] = esi;            
 ecx = eax * 0x31fef0e1;      
 ebx = ebx - ecx;          
 state[2] = ebx;            
 eax = local_u32_3;           
 eax = eax + 0xf0359c9e;      
 local_u32_0 = local_u32_0 ^ eax;  
 eax = local_u32_0;           
 return eax;
}


u32 mix_minor0(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = state[14] * 0x56be2363;   
 edx = state[12];            
 edx = ROR(edx, ecx);          
 state[12] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x664095b;       
 eax = eax + 0x8e3a5a95;      
 return eax;
}


u32 mix_minor1(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = extra_state;           
 edx = state[14];            
 edx = edx ^ 0x363a614;       
 edx = edx ^ ecx;          
 state[14] = edx;            
 eax = ecx;               
 eax = eax ^ 0x5bd077b0;      
 ecx = 0xf16362eb + eax;      
 eax = ecx;               
 return eax;
}


u32 mix_minor2(u32 *state, u32 extra_state) {
 u32 eax;
 eax = state[12] * 0x4b4f2e1;    
 eax = eax * state[1];       
 state[12] = eax;            
 eax = 0x512b59f;            
 eax = eax - extra_state;      
 eax = eax ^ 0x19ec593a;      
 return eax;
}


u32 mix_minor3(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[10];            
 edx = edx + 0xfa1f1e0b;      
 edx = edx * state[2];       
 state[2] = edx;            
 eax = extra_state;           
 ecx = 0x10;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x2923ff1;       
 return eax;
}


u32 mix_minor4(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = extra_state;           
 edx = state[0];            
 edx = edx + 0x4c3ef3f1;      
 edx = edx + ecx;          
 state[0] = edx;            
 eax = ecx;               
 eax = eax ^ 0x4895f29;       
 ecx = 0xc9bb4158 + eax;      
 eax = ecx;               
 return eax;
}


u32 mix_minor5(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[19];            
 edx = edx ^ 0x43b6b05;       
 state[19] = state[19] + edx;    
 eax = 0x13b19992;           
 eax = eax - extra_state;      
 eax = eax ^ 0x638f13d;       
 return eax;
}


u32 mix_minor6(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[4];            
 ecx = 0x12;              
 edx = ROR(edx, ecx);          
 state[18] = state[18] - edx;    
 eax = 0x51aad8a1;           
 eax = eax - extra_state;      
 eax = eax ^ 0x14e686df;      
 return eax;
}


u32 mix_minor7(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[10];            
 edx = edx + 0xfc9be92d;      
 state[0] = state[0] & edx;     
 eax = 0x228934d9;           
 eax = eax - extra_state;      
 eax = eax ^ 0xaaa8330;       
 return eax;
}


u32 mix_minor8(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 edx = extra_state;           
 ecx = edx;               
 ecx = ecx + 0x8;          
 ebx = state[19];            
 ebx = ROL(ebx, ecx);          
 state[19] = ebx;            
 eax = 0xb3c2ca;            
 eax = eax - edx;          
 eax = eax ^ 0x77351c5;       
 return eax;
}


u32 mix_minor9(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx;
 ebx = extra_state;           
 eax = ebx;               
 eax = eax ^ 0x3d1f0efd;      
 state[8] = state[8] - eax;     
 eax = ebx;               
 ecx = 0x2;               
 eax = ROL(eax, ecx);          
 eax = eax ^ 0xad69b71;       
 return eax;
}


u32 mix_minor10(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[3];            
 edx = edx + 0xbe5fec7d;      
 state[9] = state[9] ^ edx;     
 eax = extra_state;           
 ecx = 0x13;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x240e2304;      
 return eax;
}


u32 mix_minor11(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[15];            
 edx = edx | 0x46afede0;       
 edx = edx * state[6];       
 state[6] = edx;            
 eax = extra_state;           
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x41033952;      
 return eax;
}


u32 mix_minor12(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[6] * 0x1b677cc8;    
 state[17] = state[17] - edx;    
 eax = 0x35973850;           
 eax = eax - extra_state;      
 eax = eax ^ 0x2ecb6416;      
 return eax;
}


u32 mix_minor13(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx, esi;
 edx = extra_state;           
 ebx = edx;               
 ecx = 0xd;               
 ebx = ROL(ebx, ecx);          
 esi = ebx;               
 ebx = state[10];            
 ecx = esi;               
 ebx = ROL(ebx, ecx);          
 state[10] = ebx;            
 eax = 0x4e986334;           
 eax = eax - edx;          
 eax = eax ^ 0x9034f65;       
 return eax;
}


u32 mix_minor14(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx;
 ebx = extra_state;           
 eax = ebx;               
 ecx = 0x17;              
 eax = ROR(eax, ecx);          
 state[17] = state[17] ^ eax;    
 eax = 0x440a4357;           
 eax = eax - ebx;          
 eax = eax ^ 0x1c5563a7;      
 return eax;
}


u32 mix_minor15(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = extra_state;           
 edx = 0x20c85ea + ebx;       
 state[4] = state[4] ^ edx;     
 eax = ebx;               
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 ebx = 0x35e45ea9 + eax;      
 eax = ebx;               
 return eax;
}


u32 mix_minor16(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[15];            
 edx = edx + 0xfc471d2b;      
 state[14] = state[14] & edx;    
 eax = extra_state;           
 eax = eax ^ 0x1487f2a3;      
 eax = eax + 0xfd06c77c;      
 return eax;
}


u32 mix_minor17(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[19];            
 edx = edx + 0x24a7d94d;      
 edx = edx + state[16];       
 state[19] = edx;            
 eax = extra_state;           
 ecx = 0x15;              
 eax = ROR(eax, ecx);          
 eax = eax + 0xe32f226;       
 return eax;
}


u32 mix_minor18(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[0];            
 edx = edx ^ 0x48ad05f2;      
 edx = edx * state[15];       
 state[15] = edx;            
 eax = extra_state;           
 ecx = 0x13;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x318dd615;      
 return eax;
}


u32 mix_minor19(u32 *state, u32 extra_state) {
 u32 eax, ecx;
 ecx = extra_state;           
 eax = state[9];            
 eax = eax + 0x14035bf;       
 eax = eax + ecx;          
 state[9] = eax;            
 eax = 0x227b94a;            
 eax = eax - ecx;          
 eax = eax ^ 0x204260ef;      
 return eax;
}


u32 mix_minor20(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = extra_state;           
 edx = ecx;               
 edx = edx ^ 0x1aff70c8;      
 edx = edx * state[3];       
 state[3] = edx;            
 eax = ecx;               
 eax = eax ^ 0x21e79f9c;      
 ecx = 0xfacd3341 + eax;      
 eax = ecx;               
 return eax;
}


u32 mix_minor21(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[16];            
 edx = edx + 0xbb834311;      
 edx = edx - state[4];       
 state[16] = edx;            
 eax = extra_state;           
 ecx = 0x19;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x42832644;      
 return eax;
}


u32 mix_minor22(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[4];            
 ecx = 0x1a;              
 edx = ROL(edx, ecx);          
 state[8] = state[8] + edx;     
 eax = extra_state;           
 ecx = ecx ^ ecx;          
 eax = ROR(eax, ecx);          
 eax = eax + 0x2b8dc7d;       
 return eax;
}


u32 mix_minor23(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[18];            
 edx = edx + 0xac048a2;       
 edx = edx * state[13];       
 state[13] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x2482a937;      
 eax = eax + 0xbc2d41cd;      
 return eax;
}


u32 mix_minor24(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = extra_state;           
 edx = 0xd14ae1a1 + ebx;      
 state[1] = state[1] & edx;     
 eax = ebx;               
 ecx = 0x16;              
 eax = ROR(eax, ecx);          
 ebx = 0x20afd9af + eax;      
 eax = ebx;               
 return eax;
}


u32 mix_minor25(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[18];            
 edx = edx + 0xe832eb88;      
 state[16] = state[16] & edx;    
 eax = 0x131fe3c7;           
 eax = eax - extra_state;      
 eax = eax ^ 0x1627530d;      
 return eax;
}


u32 mix_minor26(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[4];            
 edx = edx + 0xe6f17893;      
 edx = edx - state[1];       
 state[4] = edx;            
 eax = extra_state;           
 ecx = 0x7;               
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x13dd3587;      
 return eax;
}


u32 mix_minor27(u32 *state, u32 extra_state) {
 u32 eax;
 eax = state[7];            
 eax = eax | 0x17b60bb5;       
 eax = eax * state[6];       
 state[6] = eax;            
 eax = 0x19d8c604;           
 eax = eax - extra_state;      
 eax = eax ^ 0xe42200d;       
 return eax;
}


u32 mix_minor28(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[12];            
 ecx = 0x10;              
 edx = ROL(edx, ecx);          
 state[15] = state[15] + edx;    
 eax = extra_state;           
 ecx = 0x9;               
 eax = ROR(eax, ecx);          
 eax = eax + 0xd8a0589;       
 return eax;
}


u32 mix_minor29(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[10];            
 edx = edx + 0xfd7af7e;       
 state[6] = state[6] & edx;     
 eax = extra_state;           
 ecx = 0x1e;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x109b5315;      
 return eax;
}


u32 mix_minor30(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = state[18];            
 ebx = ebx & 0x82;         
 edx = state[7];            
 ecx = ebx;               
 edx = ROR(edx, ecx);          
 state[7] = edx;            
 eax = extra_state;           
 ecx = 0x1;               
 eax = ROR(eax, ecx);          
 eax = eax + 0x54eddd3;       
 return eax;
}


u32 mix_minor31(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = state[7];            
 ecx = ecx ^ 0x43;         
 edx = state[17];            
 edx = ROR(edx, ecx);          
 state[17] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x79861c0;       
 eax = eax + 0xf2cdc9f1;      
 return eax;
}


u32 mix_minor32(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx;
 ebx = extra_state;           
 eax = ebx;               
 eax = eax ^ 0x1f99dc87;      
 state[11] = state[11] + eax;    
 eax = ebx;               
 ecx = 0x1b;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x3d72f5a;       
 return eax;
}


u32 mix_minor33(u32 *state, u32 extra_state) {
 u32 eax, ecx;
 ecx = extra_state;           
 eax = state[8];            
 eax = eax + 0x5a7c4580;      
 eax = eax + ecx;          
 state[8] = eax;            
 eax = 0x581851ac;           
 eax = eax - ecx;          
 eax = eax ^ 0x23f739f;       
 return eax;
}


u32 mix_minor34(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[8];            
 edx = edx + 0xeee530d5;      
 state[0] = state[0] ^ edx;     
 eax = extra_state;           
 ecx = 0x18;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x197f0302;      
 return eax;
}


u32 mix_minor35(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[10];            
 edx = edx + 0xc484cfa2;      
 edx = edx + state[1];       
 state[10] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x238547f5;      
 eax = eax + 0x92035b81;      
 return eax;
}


u32 mix_minor36(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[5];            
 edx = edx ^ 0x19a836dc;      
 state[16] = state[16] + edx;    
 eax = 0x198f4671;           
 eax = eax - extra_state;      
 eax = eax ^ 0x2709d4d9;      
 return eax;
}


u32 mix_minor37(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = extra_state;           
 edx = ebx;               
 edx = edx & 0x21e847a;       
 state[12] = state[12] + edx;    
 eax = ebx;               
 ecx = 0x1a;              
 eax = ROR(eax, ecx);          
 ebx = 0x1680fdf4 + eax;      
 eax = ebx;               
 return eax;
}


u32 mix_minor38(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[17];            
 edx = edx + 0xd68a11c3;      
 edx = edx + state[7];       
 state[17] = edx;            
 eax = extra_state;           
 ecx = 0x5;               
 eax = ROL(eax, ecx);          
 eax = eax ^ 0xe4f533;       
 return eax;
}


u32 mix_minor39(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[7];            
 ecx = 0x13;              
 edx = ROL(edx, ecx);          
 state[17] = state[17] + edx;    
 eax = extra_state;           
 eax = eax ^ 0x273f3b29;      
 eax = eax + 0xfd93b5dd;      
 return eax;
}


u32 mix_minor40(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx;
 ebx = extra_state;           
 eax = state[14];            
 eax = eax + 0xe6f27e4;       
 eax = eax - ebx;          
 state[14] = eax;            
 eax = ebx;               
 ecx = 0x14;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x73025ab;       
 return eax;
}


u32 mix_minor41(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[6] * 0x368eaf4e;    
 state[18] = state[18] - edx;    
 eax = extra_state;           
 ecx = 0x14;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x130d0502;      
 return eax;
}


u32 mix_minor42(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = state[7];            
 ebx = ebx ^ 0xc3;         
 edx = state[2];            
 ecx = ebx;               
 edx = ROR(edx, ecx);          
 state[2] = edx;            
 eax = extra_state;           
 ecx = 0xc;               
 eax = ROR(eax, ecx);          
 eax = eax + 0x201c1076;      
 return eax;
}


u32 mix_minor43(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = extra_state;           
 edx = ecx;               
 edx = edx & 0x9706840;       
 state[3] = state[3] + edx;     
 eax = ecx;               
 eax = eax ^ 0x174accb6;      
 ecx = 0xd07f9a17 + eax;      
 eax = ecx;               
 return eax;
}


u32 mix_minor44(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[5];            
 edx = edx + 0xda7c6c8e;      
 state[19] = state[19] | edx;    
 eax = extra_state;           
 eax = eax ^ 0xe34d425;       
 eax = eax + 0xf8cffbf1;      
 return eax;
}


u32 mix_minor45(u32 *state, u32 extra_state) {
 u32 eax, ecx;
 eax = state[2];            
 ecx = 0xc;               
 eax = ROR(eax, ecx);          
 eax = eax * state[6];       
 state[6] = eax;            
 eax = 0x188c4226;           
 eax = eax - extra_state;      
 eax = eax ^ 0x2ce236d2;      
 return eax;
}


u32 mix_minor46(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[14];            
 edx = edx + 0xf655a040;      
 edx = edx + state[18];       
 state[14] = edx;            
 eax = extra_state;           
 ecx = 0x14;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x20b4a6d5;      
 return eax;
}


u32 mix_minor47(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = extra_state;           
 edx = ebx;               
 ecx = 0x1e;              
 edx = ROL(edx, ecx);          
 state[5] = state[5] + edx;     
 eax = ebx;               
 eax = eax ^ 0x706c75b;       
 ebx = 0xfe1a3692 + eax;      
 eax = ebx;               
 return eax;
}


u32 mix_minor48(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[19] * 0x251df1bd;   
 state[11] = state[11] + edx;    
 eax = extra_state;           
 eax = eax ^ 0xacb4dad;       
 eax = eax + 0xdee23e79;      
 return eax;
}


u32 mix_minor49(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = extra_state;           
 edx = state[3];            
 edx = edx + 0x1805a906;      
 edx = edx - ebx;          
 state[3] = edx;            
 eax = ebx;               
 ecx = 0x12;              
 eax = ROR(eax, ecx);          
 ebx = 0x1ded1e47 + eax;      
 eax = ebx;               
 return eax;
}


u32 mix_minor50(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[0];            
 edx = edx ^ 0x51a859c;       
 state[11] = state[11] - edx;    
 eax = extra_state;           
 ecx = 0x1e;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x1769b7fe;      
 return eax;
}


u32 mix_minor51(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[18];            
 edx = edx + 0xdcccfc5;       
 edx = edx + state[6];       
 state[18] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x2439fa9;       
 eax = eax + 0xfdbe08f0;      
 return eax;
}


u32 mix_minor52(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[18];            
 edx = edx ^ 0x39848960;      
 state[16] = state[16] - edx;    
 eax = extra_state;           
 eax = eax ^ 0x268cbfd2;      
 eax = eax + 0xea8294f9;      
 return eax;
}


u32 mix_minor53(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[19];            
 edx = edx + 0x1a6f3b29;      
 state[14] = state[14] ^ edx;    
 eax = extra_state;           
 eax = eax ^ 0x23e16e7a;      
 eax = eax + 0xe13581ad;      
 return eax;
}


u32 mix_minor54(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 edx = extra_state;           
 ecx = edx;               
 ecx = ecx ^ 0xe1;         
 ebx = state[16];            
 ebx = ROL(ebx, ecx);          
 state[16] = ebx;            
 eax = 0x18ffcb6f;           
 eax = eax - edx;          
 eax = eax ^ 0x5d0fc7a2;      
 return eax;
}


u32 mix_minor55(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[5];            
 edx = edx + 0x4ef1335a;      
 state[12] = state[12] & edx;    
 eax = extra_state;           
 eax = eax ^ 0xad9bb7a;       
 eax = eax + 0x9d395ad7;      
 return eax;
}


u32 mix_minor56(u32 *state, u32 extra_state) {
 u32 eax;
 eax = state[13];            
 eax = eax + 0xdb61abf8;      
 eax = eax * state[14];       
 state[14] = eax;            
 eax = 0x297788d0;           
 eax = eax - extra_state;      
 eax = eax ^ 0x560bd89c;      
 return eax;
}


u32 mix_minor57(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[19] * 0x378f67;    
 state[18] = state[18] ^ edx;    
 eax = 0x794c54;            
 eax = eax - extra_state;      
 eax = eax ^ 0x18421e28;      
 return eax;
}


u32 mix_minor58(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[4] * 0x2dd2a2fe;    
 state[18] = state[18] ^ edx;    
 eax = extra_state;           
 eax = eax ^ 0x4bbc1ced;      
 eax = eax + 0xea99d318;      
 return eax;
}


u32 mix_minor59(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 ecx = extra_state;           
 edx = state[14];            
 edx = edx + 0x49fa4a75;      
 edx = edx + ecx;          
 state[14] = edx;            
 eax = ecx;               
 eax = eax ^ 0x25a4da18;      
 ecx = 0xf5889a55 + eax;      
 eax = ecx;               
 return eax;
}


u32 mix_minor60(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[16];            
 edx = edx + 0xe357b476;      
 edx = edx - state[4];       
 state[16] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x5cc667;       
 eax = eax + 0xc5617b0e;      
 return eax;
}


u32 mix_minor61(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[6];            
 eax = 8 * edx;             
 eax = eax - edx;          
 eax = eax << 7;            
 eax = eax + edx;          
 eax = edx + 8 * eax;          
 eax = eax << 8;            
 eax = eax + edx;          
 eax = edx + 2 * eax;          
 eax = eax * state[16];       
 state[6] = eax;            
 eax = extra_state;           
 ecx = 0x19;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x24d98516;      
 return eax;
}


u32 mix_minor62(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[11];            
 ecx = 0x18;              
 edx = ROR(edx, ecx);          
 state[10] = state[10] | edx;    
 eax = extra_state;           
 ecx = 0x1a;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x3bbefcc;       
 return eax;
}


u32 mix_minor63(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[5];            
 edx = edx + 0x147c80d5;      
 state[10] = state[10] ^ edx;    
 eax = extra_state;           
 eax = eax ^ 0xcf4b321;       
 eax = eax + 0xd97347a8;      
 return eax;
}


u32 mix_minor64(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[3] * 0x27139980;    
 state[16] = state[16] ^ edx;    
 eax = extra_state;           
 ecx = 0x1e;              
 eax = ROR(eax, ecx);          
 eax = eax + 0x1612e23e;      
 return eax;
}


u32 mix_minor65(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx;
 ebx = extra_state;           
 eax = 0xc5c4d698 + ebx;      
 eax = eax * state[4];       
 state[4] = eax;            
 eax = ebx;               
 ecx = 0x15;              
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x1ab6a65;       
 return eax;
}


u32 mix_minor66(u32 *state, u32 extra_state) {
 u32 eax, ebx, ecx, edx;
 ebx = state[17];            
 ebx = ebx & 0x7f;         
 edx = state[15];            
 ecx = ebx;               
 edx = ROR(edx, ecx);          
 state[15] = edx;            
 eax = extra_state;           
 ecx = 0x2;               
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x3829a80;       
 return eax;
}


u32 mix_minor67(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[19];            
 ecx = 0x6;               
 edx = ROL(edx, ecx);          
 state[14] = state[14] & edx;    
 eax = 0x5a67966;            
 eax = eax - extra_state;      
 eax = eax ^ 0x2f9e0d;       
 return eax;
}


u32 mix_minor68(u32 *state, u32 extra_state) {
 u32 eax, edx;
 edx = state[8] * 0x1a4c02dd;    
 edx = edx * state[0];       
 state[8] = edx;            
 eax = extra_state;           
 eax = eax ^ 0x3f40483d;      
 eax = eax + 0xfa230818;      
 return eax;
}


u32 mix_minor69(u32 *state, u32 extra_state) {
 u32 eax, ecx, edx;
 edx = state[14];            
 edx = edx + 0xfddb63a2;      
 state[16] = state[16] ^ edx;    
 eax = extra_state;           
 ecx = 0x1;               
 eax = ROL(eax, ecx);          
 eax = eax ^ 0x3e03e182;      
 return eax;
}

u32 call(u32 addr, u32 *state, u32 extra_state)
{
 switch(addr) {
  case 0x080846e8:
   return mix_major0(state,extra_state);
   break;
  case 0x08068ee8:
   return mix_major1(state,extra_state);
   break;
  case 0x08051de8:
   return mix_major2(state,extra_state);
   break;
  case 0x080523b0:
   return mix_major3(state,extra_state);
   break;
  case 0x08054bec:
   return mix_major4(state,extra_state);
   break;
  case 0x08056e88:
   return mix_major5(state,extra_state);
   break;
  case 0x08067e18:
   return mix_major6(state,extra_state);
   break;
  case 0x08071c98:
   return mix_major7(state,extra_state);
   break;
  case 0x08072228:
   return mix_major8(state,extra_state);
   break;
  case 0x080750ec:
   return mix_major9(state,extra_state);
   break;
  case 0x0807791c:
   return mix_major10(state,extra_state);
   break;
  case 0x0808bbc8:
   return mix_major11(state,extra_state);
   break;
  case 0x0808dd58:
   return mix_major12(state,extra_state);
   break;
  case 0x0808e148:
   return mix_major13(state,extra_state);
   break;
  case 0x0809e318:
   return mix_major14(state,extra_state);
   break;
  case 0x0808ccc8:
   return mix_major15(state,extra_state);
   break;
  case 0x0806c054:
   return mix_major16(state,extra_state);
   break;
  case 0x0805eb14:
   return mix_major17(state,extra_state);
   break;
  case 0x080872c4:
   return mix_major18(state,extra_state);
   break;
  case 0x08067aec:
   return mix_major19(state,extra_state);
   break;
  case 0x0808d094:
   return mix_major20(state,extra_state);
   break;
  case 0x0807f868:
   return mix_major21(state,extra_state);
   break;
  case 0x0807d85c:
   return mix_major22(state,extra_state);
   break;
  case 0x0805d780:
   return mix_major23(state,extra_state);
   break;
  case 0x08060768:
   return mix_major24(state,extra_state);
   break;
  case 0x08054bc8:
   return mix_minor0(state,extra_state);
   break;
  case 0x08057440:
   return mix_minor1(state,extra_state);
   break;
  case 0x0805d738:
   return mix_minor2(state,extra_state);
   break;
  case 0x0805dd74:
   return mix_minor3(state,extra_state);
   break;
  case 0x0805dd98:
   return mix_minor4(state,extra_state);
   break;
  case 0x0805ddc0:
   return mix_minor5(state,extra_state);
   break;
  case 0x0805ee44:
   return mix_minor6(state,extra_state);
   break;
  case 0x08060d78:
   return mix_minor7(state,extra_state);
   break;
  case 0x08060d98:
   return mix_minor8(state,extra_state);
   break;
  case 0x08067ac8:
   return mix_minor9(state,extra_state);
   break;
  case 0x080694f4:
   return mix_minor10(state,extra_state);
   break;
  case 0x08069514:
   return mix_minor11(state,extra_state);
   break;
  case 0x08069538:
   return mix_minor12(state,extra_state);
   break;
  case 0x0806bfb0:
   return mix_minor13(state,extra_state);
   break;
  case 0x0806c008:
   return mix_minor14(state,extra_state);
   break;
  case 0x0806c02c:
   return mix_minor15(state,extra_state);
   break;
  case 0x0806c3b8:
   return mix_minor16(state,extra_state);
   break;
  case 0x0806c400:
   return mix_minor17(state,extra_state);
   break;
  case 0x0806c424:
   return mix_minor18(state,extra_state);
   break;
  case 0x0807509c:
   return mix_minor19(state,extra_state);
   break;
  case 0x080750c0:
   return mix_minor20(state,extra_state);
   break;
  case 0x080756ac:
   return mix_minor21(state,extra_state);
   break;
  case 0x08077ecc:
   return mix_minor22(state,extra_state);
   break;
  case 0x08077eec:
   return mix_minor23(state,extra_state);
   break;
  case 0x08077f14:
   return mix_minor24(state,extra_state);
   break;
  case 0x08077f60:
   return mix_minor25(state,extra_state);
   break;
  case 0x0807d7d4:
   return mix_minor26(state,extra_state);
   break;
  case 0x0807d7f8:
   return mix_minor27(state,extra_state);
   break;
  case 0x0807d81c:
   return mix_minor28(state,extra_state);
   break;
  case 0x0807de40:
   return mix_minor29(state,extra_state);
   break;
  case 0x0807fe6c:
   return mix_minor30(state,extra_state);
   break;
  case 0x080871ec:
   return mix_minor31(state,extra_state);
   break;
  case 0x08087210:
   return mix_minor32(state,extra_state);
   break;
  case 0x08087234:
   return mix_minor33(state,extra_state);
   break;
  case 0x08087280:
   return mix_minor34(state,extra_state);
   break;
  case 0x080872a0:
   return mix_minor35(state,extra_state);
   break;
  case 0x0808baa8:
   return mix_minor36(state,extra_state);
   break;
  case 0x0808bacc:
   return mix_minor37(state,extra_state);
   break;
  case 0x0808baf4:
   return mix_minor38(state,extra_state);
   break;
  case 0x0808bb18:
   return mix_minor39(state,extra_state);
   break;
  case 0x0808bb58:
   return mix_minor40(state,extra_state);
   break;
  case 0x0809e8cc:
   return mix_minor41(state,extra_state);
   break;
  case 0x0809e938:
   return mix_minor42(state,extra_state);
   break;
  case 0x0809e8ec:
   return mix_minor43(state,extra_state);
   break;
  case 0x080778f8:
   return mix_minor44(state,extra_state);
   break;
  case 0x08060dc0:
   return mix_minor45(state,extra_state);
   break;
  case 0x0805eaf0:
   return mix_minor46(state,extra_state);
   break;
  case 0x0808bb80:
   return mix_minor47(state,extra_state);
   break;
  case 0x08067aa8:
   return mix_minor48(state,extra_state);
   break;
  case 0x08087178:
   return mix_minor49(state,extra_state);
   break;
  case 0x0809e2f8:
   return mix_minor50(state,extra_state);
   break;
  case 0x080756d0:
   return mix_minor51(state,extra_state);
   break;
  case 0x0809e914:
   return mix_minor52(state,extra_state);
   break;
  case 0x0806c3dc:
   return mix_minor53(state,extra_state);
   break;
  case 0x08087258:
   return mix_minor54(state,extra_state);
   break;
  case 0x0807fe94:
   return mix_minor55(state,extra_state);
   break;
  case 0x0805d75c:
   return mix_minor56(state,extra_state);
   break;
  case 0x0807d83c:
   return mix_minor57(state,extra_state);
   break;
  case 0x0808bba8:
   return mix_minor58(state,extra_state);
   break;
  case 0x08084cb4:
   return mix_minor59(state,extra_state);
   break;
  case 0x08057468:
   return mix_minor60(state,extra_state);
   break;
  case 0x0806bf78:
   return mix_minor61(state,extra_state);
   break;
  case 0x0806c398:
   return mix_minor62(state,extra_state);
   break;
  case 0x08077f3c:
   return mix_minor63(state,extra_state);
   break;
  case 0x0805ee24:
   return mix_minor64(state,extra_state);
   break;
  case 0x080871c4:
   return mix_minor65(state,extra_state);
   break;
  case 0x0806bfe0:
   return mix_minor66(state,extra_state);
   break;
  case 0x080871a4:
   return mix_minor67(state,extra_state);
   break;
	case 0x0808d330:
   return mix_minor68(state,extra_state);
   break;
  case 0x0808bb38:
   return mix_minor69(state,extra_state);
   break;
  }
  return 0;
}

