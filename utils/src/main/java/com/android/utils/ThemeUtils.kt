package com.android.utils

import android.content.Context
import android.content.res.Configuration
import android.os.Build

/**
 * @author: QiuYaWei
 *$Revision.1.0\$
 * Date: 2025/4/23
 * Description: [主题样式帮助类]
 */
object ThemeUtils {
    /***
     * 判断是否处于黑夜模式
     */
    fun isNightModeEnabled(context: Context): Boolean {
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            context.resources.configuration.isNightModeActive
        } else {
            val nightModeFlags =
                context.resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK
            nightModeFlags == Configuration.UI_MODE_NIGHT_YES
        }
    }
}