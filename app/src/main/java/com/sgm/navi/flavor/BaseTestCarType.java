package com.sgm.navi.flavor;

import android.util.Log;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @Description TODO
 * @Author lvww
 * @date 2025/1/2
 */
public class BaseTestCarType {
    public BaseTestCarType() {
        //createDimens(320);
    }

    private void createDimens(int dpi) {
        double density = 1.25;
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < 4000; i++) {
            BigDecimal bigDecimal = new BigDecimal(i / density);
            BigDecimal rounded  = bigDecimal.setScale(2, RoundingMode.HALF_UP).stripTrailingZeros();
            stringBuilder.append("<dimen name=\"dp_")
                    .append(i)
                    .append("\">")
                    .append(rounded)
                    .append("dp")
                    .append("</dimen>")
                    .append("\n");
        }
        Log.e("lvww", stringBuilder.toString());
    }
}
