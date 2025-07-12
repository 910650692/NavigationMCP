package com.android.utils.theme

import android.content.Context
import android.content.res.Configuration
import android.os.Build
import androidx.appcompat.app.AppCompatDelegate
import com.android.utils.log.Logger

/**
 * @author: QiuYaWei
 *$Revision.1.0\$
 * Date: 2025/4/23
 * Description: [主题样式帮助类]
 */
object ThemeUtils {
    private var currentUiMode = 0;

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

    fun getCurrentUiMode(): Int {
        Logger.d("ThemeStyle", "current ui mode: $currentUiMode")
        return currentUiMode
    }

    fun setCurrentUiMode(uiMode: Int) {
        currentUiMode = uiMode
        Logger.d("ThemeStyle", "set ui mode: $currentUiMode")
    }

    fun setAppThemeFollowSystem(context: Context) {
        currentUiMode = context.resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK
        Logger.d("ThemeStyle", "current system ui mode: $currentUiMode")
        AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM)
    }

    fun isNeedToChangeTheme(context: Context, displayUiMode: Int): Boolean {
        var currentThem = getSystemCurrentUiMode(context)
        var changeTheme = displayUiMode != currentThem
        setCurrentUiMode(currentThem)
        Logger.d("ThemeStyle", "is need to change theme: $changeTheme")
        return true
    }

    fun getSystemCurrentUiMode(context: Context): Int {
        return context.resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK
    }

    fun getCurrentTheme(context: Context): ThemeType {
        return if (isNightModeEnabled(context)) ThemeType.NIGHT else ThemeType.DAY
    }
}