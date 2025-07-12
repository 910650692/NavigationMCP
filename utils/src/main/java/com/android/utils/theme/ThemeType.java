package com.android.utils.theme;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/23
 * Description: [在这里描述文件功能]
 */
public enum ThemeType {
    DAY,
    NIGHT,
    AUTO,
    UN_KNOWN;

    // 16：自动模式，默认态； 17：日间模式； 18：夜间模式
    public static int getThemeValueByType(ThemeType type) {
        return switch (type) {
            case DAY -> 17;
            case NIGHT -> 18;
            case AUTO -> 16;
            default -> 17;
        };
    }

    public static ThemeType getThemeTypeByValue(int value) {
        return switch (value) {
            case 17 -> DAY;
            case 18 -> NIGHT;
            case 16 -> AUTO;
            default -> UN_KNOWN;
        };
    }
}
