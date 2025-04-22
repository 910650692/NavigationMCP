package com.fy.navi.service.adapter.engine.bls;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import java.lang.reflect.UndeclaredThrowableException;
import java.security.GeneralSecurityException;

public final class TOTPUtils {

    private TOTPUtils() {

    }

    public static TOTPUtils getInstance() {
        return Helper.INSTANCE;
    }

    private final static class Helper {
        public final static TOTPUtils INSTANCE = new TOTPUtils();
    }

    /**
     * :
     */
    private static final long STEP = 60000;
    /**
     * [1-8]
     */
    private static final int CODE_DIGITS = 8;
    /**
     *
     */
    private static final long INITIAL_TIME = 0;
    /**
     *
     */
    private static final long FLEXIBILIT_TIME = 60000;
    /**
     *
     */
    private static final int[] DIGITS_POWER = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};
    /**
     *
     */
    private static final int KEY_LENGTH = 32;

    /**
     * keyAuthCode,
     *
     * @param key       key
     * @param timeStamp timeStamp
     * @return authCode
     */
    public String generateMyTOTP(final String key, final Long timeStamp) {
        if (key.isEmpty()) {
            throw new RuntimeException("");
        }
        if (key.length() < KEY_LENGTH) {
            throw new RuntimeException(":" + KEY_LENGTH);
        }
        return generateTOTP256(key, String.valueOf(timeFactor(timeStamp)));
    }

    /**
     * keyAuthCode
     *
     * @param key key
     * @return authCode
     */
    public static String generateMyTOTP(final String key) {
        if (key.isEmpty()) {
            throw new RuntimeException("");
        }
        if (key.length() < KEY_LENGTH) {
            throw new RuntimeException(":" + KEY_LENGTH);
        }
        return generateTOTP256(key, String.valueOf(timeFactor(System.currentTimeMillis())));
    }

    /**
     * @param key  key
     * @param totp totp
     * @return boolean
     */
    public static boolean verifyTOTPRigidity(final String key, final String totp) {
        return generateMyTOTP(key).equals(totp);
    }

    /**
     * @param key  key
     * @param totp totp
     * @return boolean
     */
    public static boolean verifyTOTPFlexibility(final String key, final String totp) {
        final long currentTime = System.currentTimeMillis();
        final String tempTotp = generateTOTP256(key, String.valueOf(timeFactor(currentTime)));
        if (tempTotp.equals(totp)) {
            return true;
        }
        //
        final String tempTotpSub = generateTOTP256(key, String.valueOf(timeFactor(currentTime - FLEXIBILIT_TIME)));
        if (tempTotpSub.equals(totp)) {
            return true;
        }
        //
        final String tempTotpAdd = generateTOTP256(key, String.valueOf(timeFactor(currentTime + FLEXIBILIT_TIME)));
        return tempTotpAdd.equals(totp);
    }

    /**
     * []
     *
     * @param key  key
     * @param time time
     * @param totp totp
     * @return boolean
     */
    public static boolean verifyTOTPByTime(final String key, final long time, final String totp) {
        final long currentTime = System.currentTimeMillis();
        if (Math.abs(currentTime - time) <= FLEXIBILIT_TIME) {
            final String tempTotp = generateTOTP256(key, String.valueOf(timeFactor(time)));
            return tempTotp.equals(totp);
        } else {
            return false;
        }
    }

    /**
     * @param targetTime targetTime
     * @return long
     */
    private static long timeFactor(final long targetTime) {
        return (targetTime - INITIAL_TIME) / STEP;
    }

    /**
     * @param crypto   crypto
     * @param keyBytes keyBytes
     * @param text     text
     * @return byte[]
     */
    private static byte[] hmacSha(final String crypto, final byte[] keyBytes, final byte[] text) {
        try {
            final Mac hmac;
            hmac = Mac.getInstance(crypto);
            final SecretKeySpec macKey = new SecretKeySpec(keyBytes, "RAW");
            hmac.init(macKey);
            return hmac.doFinal(text);
        } catch (GeneralSecurityException gse) {
            throw new UndeclaredThrowableException(gse);
        }
    }

    /**
     * @param hex hex
     * @return byte[]
     */
    public static byte[] hexStr2Bytes(final String hex) {
        int num = Integer.parseInt(hex);
        final byte[] ret = new byte[8];
        for (int i = 7; i >= 0; --i) {
            ret[i] = (byte) (num & 255);
            num = num >> 8;
        }
        return ret;
    }

    /**
     * @param key  key
     * @param time time
     * @return string
     */
    private static String generateTOTP(final String key, final String time) {
        return generateTOTP(key, time, "HmacSHA1");
    }

    /**
     * @param key  key
     * @param time time
     * @return string
     */
    private static String generateTOTP256(final String key, final String time) {
        return generateTOTP(key, time, "HmacSHA256");
    }

    /**
     * @param key  key
     * @param time time
     * @return string
     */
    private static String generateTOTP512(final String key, final String time) {
        return generateTOTP(key, time, "HmacSHA512");
    }

    /**
     * @param key key
     * @param time time
     * @param crypto crypto
     * @return string
     */
    private static String generateTOTP(final String key, final String time, final String crypto) {
        final byte[] msg = hexStr2Bytes(time);
        final byte[] k = key.getBytes();
        final byte[] hash = hmacSha(crypto, k, msg);
        return truncate(hash);
    }

    /**
     * @param target 20
     * @return String
     */
    private static String truncate(final byte[] target) {
        final StringBuilder result;
        final int offset = target[target.length - 1] & 0xf;
        final int binary = ((target[offset] & 0x7f) << 24)
                | ((target[offset + 1] & 0xff) << 16)
                | ((target[offset + 2] & 0xff) << 8) | (target[offset + 3] & 0xff);
        final int otp = binary % DIGITS_POWER[CODE_DIGITS];
        result = new StringBuilder(Integer.toString(otp));
        while (result.length() < CODE_DIGITS) {
            result.insert(0, "0");
        }
        return result.toString();
    }

}
