#ifndef PARSLEY_BITSTREAM_H
#define PARSLEY_BITSTREAM_H

#include <stdbool.h>
#include <stdint.h>

// TODO: split into bitstream.h and bitstream_p.h

typedef struct {
    const uint8_t *cur;
    const uint8_t *end; // FIXME: store the length of the stream instread of the end?
    unsigned bit;
} prl_bitstream_t;

static inline void bs_init(prl_bitstream_t *bs, const uint8_t *data, size_t length) {
    bs->cur = data;
    bs->end = data + length;
    bs->bit = 0;
}

static inline bool bs_has_bytes(prl_bitstream_t *bs, size_t bytes) {
    return bs->cur + bytes <= bs->end;
}

static inline uint8_t bs_bitmask(unsigned width) {
    return (1 << width) - 1;
}

static inline uint8_t bs_read_from_inside_byte(prl_bitstream_t *bs, unsigned width) {
    uint8_t result = (*bs->cur >> (8 - bs->bit - width)) & bs_bitmask(width);
    bs->bit += width;
    bs->cur += bs->bit / 8;
    bs->bit %= 8;

    return result;
}

// TODO: replace with different functions for 'if' and 'else' cases
static inline uint8_t bs_read_bits_to_u8(prl_bitstream_t *bs, unsigned width) {
    uint8_t result;

    if (bs->bit + width <= 8) { // inside the byte
        result = (*bs->cur >> (8 - bs->bit - width)) & bs_bitmask(width);
        bs->bit += width;
        bs->cur += bs->bit / 8;
        bs->bit %= 8;
    }
    else {
        width -= (8 - bs->bit);
        result = *bs->cur & bs_bitmask(8 - bs->bit);
        ++bs->cur;
        bs->bit = width;
        result |= *bs->cur >> (8 - width);
    }

    return result;
}

static inline uint8_t bs_read_1_byte(prl_bitstream_t *bs) {
    return *bs->cur++;
}

static inline uint8_t bs_read_high_order_bits(prl_bitstream_t *bs, unsigned width) {
    bs->bit = width;
    return *bs->cur >> (8 - width);
}

static inline uint16_t bs_read_bits_to_u16(prl_bitstream_t *bs, unsigned width) {
    uint16_t result;

    if (bs->bit > 0) {
        unsigned bits = 8 - bs->bit;
        bits = width < bits ? width : bits;
        result = bs_read_from_inside_byte(bs, bits);
        width -= bits;
    }
    else {
        result = 0;
    }

    while (width >= 8) {
        result <<= 8;
        result |= bs_read_1_byte(bs);
        width -= 8;
    }

    /* read the rest */
    if (width > 0) {
        result <<= width;
        result |= bs_read_high_order_bits(bs, width);
    }

    return result;
}

static inline uint32_t bs_read_bits_to_u32(prl_bitstream_t *bs, unsigned width) {
    uint32_t result;

    if (bs->bit > 0) {
        unsigned bits = 8 - bs->bit;
        bits = width < bits ? width : bits;
        result = bs_read_from_inside_byte(bs, bits);
        width -= bits;
    }
    else {
        result = 0;
    }

    while (width >= 8) {
        result <<= 8;
        result |= bs_read_1_byte(bs);
        width -= 8;
    }

    /* read the rest */
    if (width > 0) {
        result <<= width;
        result |= bs_read_high_order_bits(bs, width);
    }

    return result;
}

static inline uint32_t bs_read_bits_to_u64(prl_bitstream_t *bs, unsigned width) {
    uint64_t result;

    if (bs->bit > 0) {
        unsigned bits = 8 - bs->bit;
        bits = width < bits ? width : bits;
        result = bs_read_from_inside_byte(bs, bits);
        width -= bits;
    }
    else {
        result = 0;
    }

    while (width >= 8) {
        result <<= 8;
        result |= bs_read_1_byte(bs);
        width -= 8;
    }

    /* read the rest */
    if (width > 0) {
        result <<= width;
        result |= bs_read_high_order_bits(bs, width);
    }

    return result;
}

#endif // PARSLEY_BITSTREAM_H
