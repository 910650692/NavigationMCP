package com.sgm.navi.flavor;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/7/16
 */
public class Unit {
    public static void main(String[] args) {
        Unit.createDimens(200, "dp", 4000);
    }

    private static void createDimens(final int dpi, final String unit, final int size) {
        double defaultDensity = 160.0;
        double density = dpi/defaultDensity;
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < size; i++) {
            BigDecimal bigDecimal = new BigDecimal(i / density);
            BigDecimal rounded = bigDecimal.setScale(2, RoundingMode.HALF_UP).stripTrailingZeros();
            stringBuilder.append("<dimen name=")
                    .append("\"")
                    .append(unit)
                    .append("_")
                    .append(i)
                    .append("\">")
                    .append(rounded)
                    .append(unit)
                    .append("</dimen>")
                    .append("\n");
        }
        System.out.println(stringBuilder);
    }
}
