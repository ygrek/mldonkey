#include <caml/mlvalues.h>

value winmx_encode_ml(value what_v, value length_v) {

char *what = String_val(what_v);
unsigned int length = Int_val(length_v);

        int counter, x;

        for (counter = 4; counter >= 0; counter--) {

                if (counter != 4) what[0] ^= what[length-1];
                else              what[0] ^= (char)(length);

                for (x = 1; x < (int)length; x++) what[x] ^= what[x-1];
        }
return Val_unit;
}


value winmx_decode_ml(value what_v, value length_v) {

char *what = String_val(what_v);
unsigned int length = Int_val(length_v);

        int counter, x;

        for (counter = 0; counter < 5; counter++) {

                for (x = length-1; x > 0; x--) what[x] ^= what[x-1];

                if (counter != 4) what[0] ^= what[length-1];
                else              what[0] ^= (char)length;
        }
return Val_unit;

}

