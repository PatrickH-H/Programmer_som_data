void blend_v(int dim, pixel *src, pixel *dst)
{
   for (int i = 0; i < dim; i += 4) {
        // Load 4 pixels into vectors
        __m128i src_vec = _mm_loadu_si128((__m128i*)&src[i]);
        __m128i dst_vec = _mm_loadu_si128((__m128i*)&dst[i]);

        // Unpack the vectors into high and low order bits
        __m128i src_lo = _mm_unpacklo_epi8(src_vec, _mm_setzero_si128());
        //__m128i src_hi = _mm_unpackhi_epi8(src_vec, _mm_setzero_si128());
        __m128i dst_lo = _mm_unpacklo_epi8(dst_vec, _mm_setzero_si128());
        //__m128i dst_hi = _mm_unpackhi_epi8(dst_vec, _mm_setzero_si128());

        // Convert to float and find alpha value
        __m128 src_lo_f = _mm_cvtepi32_ps(_mm_unpacklo_epi16(src_lo, _mm_setzero_si128()));
        __m128 src_hi_f = _mm_cvtepi32_ps(_mm_unpackhi_epi16(src_lo, _mm_setzero_si128()));
        __m128 dst_lo_f = _mm_cvtepi32_ps(_mm_unpacklo_epi16(dst_lo, _mm_setzero_si128()));
        __m128 dst_hi_f = _mm_cvtepi32_ps(_mm_unpackhi_epi16(dst_lo, _mm_setzero_si128()));

        // Normalize (divide by 255.0)
        __m128 norm_factor = _mm_set1_ps(1.0f / 255.0f);
        src_lo_f = _mm_mul_ps(src_lo_f, norm_factor);
        src_hi_f = _mm_mul_ps(src_hi_f, norm_factor);
        dst_lo_f = _mm_mul_ps(dst_lo_f, norm_factor);
        dst_hi_f = _mm_mul_ps(dst_hi_f, norm_factor);

        // Extract alpha values
        __m128 alpha_lo = _mm_shuffle_ps(src_lo_f, src_lo_f, _MM_SHUFFLE(3, 3, 3, 3));
        __m128 alpha_hi = _mm_shuffle_ps(src_hi_f, src_hi_f, _MM_SHUFFLE(3, 3, 3, 3));
        __m128 inv_alpha_lo = _mm_sub_ps(_mm_set1_ps(1.0f), alpha_lo);
        __m128 inv_alpha_hi = _mm_sub_ps(_mm_set1_ps(1.0f), alpha_hi);

        // Blend ((src_color * alpha) + (background_color * (1 - Alpha)))
        __m128 blend_lo = _mm_add_ps(_mm_mul_ps(src_lo_f, alpha_lo), _mm_mul_ps(dst_lo_f, inv_alpha_lo));
        __m128 blend_hi = _mm_add_ps(_mm_mul_ps(src_hi_f, alpha_hi), _mm_mul_ps(dst_hi_f, inv_alpha_hi));

        // Handle out-of-bounds values (clamp to [0, 255])
        blend_lo = _mm_min_ps(_mm_max_ps(blend_lo, _mm_setzero_ps()), _mm_set1_ps(1.0f));
        blend_hi = _mm_min_ps(_mm_max_ps(blend_hi, _mm_setzero_ps()), _mm_set1_ps(1.0f));

        // Convert back to integer
        blend_lo = _mm_mul_ps(blend_lo, _mm_set1_ps(255.0f));
        blend_hi = _mm_mul_ps(blend_hi, _mm_set1_ps(255.0f));
        __m128i blend_lo_i = _mm_cvtps_epi32(blend_lo);
        __m128i blend_hi_i = _mm_cvtps_epi32(blend_hi);

        // Pack the results back into the destination array
        __m128i blend_vec = _mm_packus_epi32(blend_lo_i, blend_hi_i);
        _mm_storeu_si128((__m128i*)&dst[i], blend_vec);
    }
}