/* 
 * Copyright (c) 2011 EPFL, Lausanne
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the EPFL nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL EPFL
 *  OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */

enum {
       	FIELD_SIZE = 16
};

uint8_t ff_sum_table [16][16] = {  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,  },
 { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,  },
 { 2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13,  },
 { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,  },
 { 4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11,  },
 { 5, 4, 7, 6, 1, 0, 3, 2, 13, 12, 15, 14, 9, 8, 11, 10,  },
 { 6, 7, 4, 5, 2, 3, 0, 1, 14, 15, 12, 13, 10, 11, 8, 9,  },
 { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,  },
 { 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7,  },
 { 9, 8, 11, 10, 13, 12, 15, 14, 1, 0, 3, 2, 5, 4, 7, 6,  },
 { 10, 11, 8, 9, 14, 15, 12, 13, 2, 3, 0, 1, 6, 7, 4, 5,  },
 { 11, 10, 9, 8, 15, 14, 13, 12, 3, 2, 1, 0, 7, 6, 5, 4,  },
 { 12, 13, 14, 15, 8, 9, 10, 11, 4, 5, 6, 7, 0, 1, 2, 3,  },
 { 13, 12, 15, 14, 9, 8, 11, 10, 5, 4, 7, 6, 1, 0, 3, 2,  },
 { 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1,  },
 { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,  },
 };

uint8_t ff_sub_table [16][16] = {  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,  },
 { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,  },
 { 2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13,  },
 { 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,  },
 { 4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11,  },
 { 5, 4, 7, 6, 1, 0, 3, 2, 13, 12, 15, 14, 9, 8, 11, 10,  },
 { 6, 7, 4, 5, 2, 3, 0, 1, 14, 15, 12, 13, 10, 11, 8, 9,  },
 { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,  },
 { 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7,  },
 { 9, 8, 11, 10, 13, 12, 15, 14, 1, 0, 3, 2, 5, 4, 7, 6,  },
 { 10, 11, 8, 9, 14, 15, 12, 13, 2, 3, 0, 1, 6, 7, 4, 5,  },
 { 11, 10, 9, 8, 15, 14, 13, 12, 3, 2, 1, 0, 7, 6, 5, 4,  },
 { 12, 13, 14, 15, 8, 9, 10, 11, 4, 5, 6, 7, 0, 1, 2, 3,  },
 { 13, 12, 15, 14, 9, 8, 11, 10, 5, 4, 7, 6, 1, 0, 3, 2,  },
 { 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1,  },
 { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,  },
 };

uint8_t ff_mul_table [16][16] = {  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  },
 { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,  },
 { 0, 2, 4, 6, 8, 10, 12, 14, 3, 1, 7, 5, 11, 9, 15, 13,  },
 { 0, 3, 6, 5, 12, 15, 10, 9, 11, 8, 13, 14, 7, 4, 1, 2,  },
 { 0, 4, 8, 12, 3, 7, 11, 15, 6, 2, 14, 10, 5, 1, 13, 9,  },
 { 0, 5, 10, 15, 7, 2, 13, 8, 14, 11, 4, 1, 9, 12, 3, 6,  },
 { 0, 6, 12, 10, 11, 13, 7, 1, 5, 3, 9, 15, 14, 8, 2, 4,  },
 { 0, 7, 14, 9, 15, 8, 1, 6, 13, 10, 3, 4, 2, 5, 12, 11,  },
 { 0, 8, 3, 11, 6, 14, 5, 13, 12, 4, 15, 7, 10, 2, 9, 1,  },
 { 0, 9, 1, 8, 2, 11, 3, 10, 4, 13, 5, 12, 6, 15, 7, 14,  },
 { 0, 10, 7, 13, 14, 4, 9, 3, 15, 5, 8, 2, 1, 11, 6, 12,  },
 { 0, 11, 5, 14, 10, 1, 15, 4, 7, 12, 2, 9, 13, 6, 8, 3,  },
 { 0, 12, 11, 7, 5, 9, 14, 2, 10, 6, 1, 13, 15, 3, 4, 8,  },
 { 0, 13, 9, 4, 1, 12, 8, 5, 2, 15, 11, 6, 3, 14, 10, 7,  },
 { 0, 14, 15, 1, 13, 3, 2, 12, 9, 7, 6, 8, 4, 10, 11, 5,  },
 { 0, 15, 13, 2, 9, 6, 4, 11, 1, 14, 12, 3, 8, 7, 5, 10,  },
 };

uint8_t ff_div_table [16][16] = {  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  },
 { 0, 1, 9, 14, 13, 11, 7, 6, 15, 2, 12, 5, 10, 4, 3, 8,  },
 { 0, 2, 1, 15, 9, 5, 14, 12, 13, 4, 11, 10, 7, 8, 6, 3,  },
 { 0, 3, 8, 1, 4, 14, 9, 10, 2, 6, 7, 15, 13, 12, 5, 11,  },
 { 0, 4, 2, 13, 1, 10, 15, 11, 9, 8, 5, 7, 14, 3, 12, 6,  },
 { 0, 5, 11, 3, 12, 1, 8, 13, 6, 10, 9, 2, 4, 7, 15, 14,  },
 { 0, 6, 3, 2, 8, 15, 1, 7, 4, 12, 14, 13, 9, 11, 10, 5,  },
 { 0, 7, 10, 12, 5, 4, 6, 1, 11, 14, 2, 8, 3, 15, 9, 13,  },
 { 0, 8, 4, 9, 2, 7, 13, 5, 1, 3, 10, 14, 15, 6, 11, 12,  },
 { 0, 9, 13, 7, 15, 12, 10, 3, 14, 1, 6, 11, 5, 2, 8, 4,  },
 { 0, 10, 5, 6, 11, 2, 3, 9, 12, 7, 1, 4, 8, 14, 13, 15,  },
 { 0, 11, 12, 8, 6, 9, 4, 15, 3, 5, 13, 1, 2, 10, 14, 7,  },
 { 0, 12, 6, 4, 3, 13, 2, 14, 8, 11, 15, 9, 1, 5, 7, 10,  },
 { 0, 13, 15, 10, 14, 6, 5, 8, 7, 9, 3, 12, 11, 1, 4, 2,  },
 { 0, 14, 7, 11, 10, 8, 12, 2, 5, 15, 4, 3, 6, 13, 1, 9,  },
 { 0, 15, 14, 5, 7, 3, 11, 4, 10, 13, 8, 6, 12, 9, 2, 1,  },
 };

void ff_set_c(uint8_t *vector, uint16_t pos, uint8_t value) {
	uint8_t position;

	value = value & 0xF;
	position = pos >> 1;

	if ( (pos & 0x1) == 0 ) {
		vector[position] = (value << 4) | (vector[position] & 0x0F);
	} else {
		vector[position] = value | (vector[position] & 0xF0);
	}
}

uint8_t ff_get_c(uint8_t *vector, uint16_t pos) {

        if ( (pos & 0x1) == 0) {
		return vector[pos >> 1] >> 4;
	} else {
		return vector[pos >> 1] & 0x0F;
	}
}

void ff_sum(uint8_t *vector, uint8_t *other, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a,b;
		a = ff_get_c(vector, i);
		b = ff_get_c(other, i);
		ff_set_c(vector, i, ff_sum_table[a][b]);
	}
}

void ff_mul_sum(uint8_t *vector, uint8_t *other, uint8_t scalar, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a,b;
		a = ff_get_c(vector, i);
		b = ff_mul_table[scalar][ff_get_c(other, i)];
		ff_set_c(vector, i, ff_sum_table[a][b]);
	}
}

void ff_mul_sub(uint8_t *vector, uint8_t *other, uint8_t scalar, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a,b;
		a = ff_get_c(vector, i);
		b = ff_mul_table[scalar][ff_get_c(other, i)];
		ff_set_c(vector, i, ff_sub_table[a][b]);
	}
}

void ff_div(uint8_t *vector, uint8_t scalar, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a;
		a = ff_get_c(vector, i);
		ff_set_c(vector, i, ff_div_table[a][scalar]);
	}
}

void ff_multiply(uint8_t *vector, uint8_t scalar, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a;
		a = ff_get_c(vector, i);
		ff_set_c(vector, i, ff_mul_table[a][scalar]);
	}
}


void ff_sub(uint8_t *vector, uint8_t *other, uint16_t len) {
		
	int i;
	for ( i = 0 ; i < len ; i++) {
		uint8_t a,b;
		a = ff_get_c(vector, i);
		b = ff_get_c(other, i);
		ff_set_c(vector, i, ff_sub_table[a][b]);
	}
}

bool ff_is_zero(uint8_t *vector, uint16_t len) {
	int i;          
	for ( i = 0 ; i < len ; i++) {
		if (ff_get_c(vector, i) != 0) return FALSE;
	}
	return TRUE;
}

uint16_t ff_find_pivot(uint8_t *vector, uint16_t len) {
	int i;          
	for ( i = 0 ; i < len ; i++) {
		if (ff_get_c(vector, i) != 0) return i;
	}
	return len;
}

uint16_t ff_is_canonical(uint8_t *vector, uint16_t len) {
	int i,ret = 0xFFFF;          
	for ( i = 0 ; i < len ; i++) {
		if (ff_get_c(vector, i) != 0) {
			if (ret != 0xFFFF) {
				return 0xFFFF;
			} else {
				ret = i;
			}
		}
	}
	return ret;
}

#ifdef TOSSIM
char *ff_dump_vector(char *data_str, uint8_t *vector, uint16_t len) {
	char temp[10];
	int i;

	data_str[0] = 0;

	for ( i = 0 ; i < len ; i++) {
		sprintf(temp, "%x", ff_get_c(vector, i));
		strcat(data_str, temp);
	}

	return data_str;
}
#endif
