package com.android.utils

import android.app.Activity
import android.content.Context
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Point
import android.graphics.drawable.BitmapDrawable
import android.graphics.drawable.Drawable
import android.hardware.display.DisplayManager
import android.os.Build
import android.util.DisplayMetrics
import android.util.TypedValue
import android.view.View
import android.view.WindowManager
import androidx.core.content.getSystemService
import java.io.BufferedOutputStream
import java.io.PrintStream

/**
 * @Introduce: 屏幕管理.
 * @Author: lvww
 * @Date: 2023/11/13
 */
class ScreenUtils private constructor() {
    private var mContext: Context? = null
    fun init(context: Context?) {
        mContext = context
    }

    fun clearCache() {
        mContext = null
    }

    /**
     * dp转px.
     */
    fun dp2px(dpVal: Float): Int {
        return TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP,
            dpVal,
            mContext!!.resources.displayMetrics
        ).toInt()
    }

    /**
     * dp转px.
     */
    fun dp2px(dpVal: Int): Int {
        return ConvertUtils.float2int(mContext!!.resources.displayMetrics.density * dpVal)
    }

    /**
     * sp转px.
     */
    fun sp2px(spVal: Float): Int {
        return TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_SP,
            spVal, mContext!!.resources.displayMetrics
        ).toInt()
    }

    /**
     * px转dp.
     */
    fun px2dp(pxVal: Float): Float {
        val scale = mContext!!.resources.displayMetrics.density
        return pxVal / scale
    }

    /**
     * px转dp.
     */
    fun px2dp(pxVal: Int): Int {
        return ConvertUtils.float2int(pxVal / mContext!!.resources.displayMetrics.density)
    }

    /**
     * px转sp.
     */
    fun px2sp(pxVal: Float): Float {
        return pxVal / mContext!!.resources.displayMetrics.scaledDensity
    }

    val screenWidth: Int
        get() {
            val windowmanager = mContext!!.getSystemService(Context.WINDOW_SERVICE) as WindowManager
            val display = windowmanager.defaultDisplay
            val outMetrics = DisplayMetrics()
            display.getMetrics(outMetrics)
            return outMetrics.widthPixels
        }
    val screenHeight: Int
        get() {
            val manager = mContext!!.getSystemService(Context.WINDOW_SERVICE) as WindowManager
            val dm = DisplayMetrics()
            manager.defaultDisplay.getMetrics(dm)
            return dm.heightPixels
        }

    fun setLayoutParams(view: View, width: Float, height: Float) {
        val params = view.layoutParams
        params.width = dp2px(width)
        params.height = dp2px(height)
        view.layoutParams = params
    }

    /**
     * 通过view获取当前屏幕截图.
     *
     * @param view
     * @return
     */
    fun prcScreenBitmap(view: View): Bitmap {
        val rootView = view.rootView
        rootView.isDrawingCacheEnabled = true
        rootView.buildDrawingCache()
        val bmp = rootView.drawingCache
        val width = rootView.width
        val height = rootView.height
        val bp = Bitmap.createBitmap(bmp, 0, 0, width, height)
        rootView.destroyDrawingCache()
        return bp
    }

    /**
     * 通过view获取当前屏幕截图.
     *
     * @param view
     * @return
     */
    fun prcScreenDrawable(view: View): Drawable {
        val rootView = view.rootView
        rootView.isDrawingCacheEnabled = true
        rootView.buildDrawingCache()
        val bmp = rootView.drawingCache
        val width = rootView.width
        val height = rootView.height
        val bp =
            Bitmap.createBitmap(bmp, 0, 0, width, height)
        rootView.destroyDrawingCache()
        return BitmapDrawable(mContext!!.resources, bp)
    }

    /**
     * 通过Activity获取当前屏幕截图.
     *
     * @param activity
     * @return
     */
    @Deprecated("")
    fun prcScreenBitmap(activity: Activity): Bitmap {
        val v = activity.window.decorView
        val bitmap = Bitmap.createBitmap(v.width, v.height, Bitmap.Config.ARGB_8888)
        val canvas = Canvas()
        canvas.setBitmap(bitmap)
        v.draw(canvas)
        return bitmap
    }

    /**
     * 屏幕截图 需要root权限.
     *
     * @param path
     * @return
     */
    private fun getScreenshot(path: String): String {
        var process: Process? = null
        var outputStream: PrintStream? = null
        try {
            process = Runtime.getRuntime().exec("su")
            outputStream =
                PrintStream(BufferedOutputStream(process.outputStream, 8192))
            outputStream.println("screencap -p $path")
            outputStream.flush()
            process.waitFor()
        } catch (e: Exception) {
            e.printStackTrace()
        } finally {
            outputStream?.close()
            process?.destroy()
        }
        return path
    }

    private object Helper {
        val instance = ScreenUtils()
    }

    companion object {
        fun getInstance() = Helper.instance
    }

    /**
     * 获取屏幕的真实宽度
     */
    fun getRealScreenWidth(context: Context): Int {
        val windowManager = context.getSystemService(Context.WINDOW_SERVICE) as WindowManager
        val display = windowManager.defaultDisplay
        val size = Point()
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
            display.getRealSize(size)
        } else {
            display.getSize(size)
        }
        return size.x
    }

    /**
     * 获取屏幕的真实高度
     */
    fun getRealScreenHeight(context: Context): Int {
        val windowManager = context.getSystemService(Context.WINDOW_SERVICE) as WindowManager
        val display = windowManager.defaultDisplay
        val size = Point()
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
            display.getRealSize(size)
        } else {
            display.getSize(size)
        }
        return size.y
    }

    fun getStatusBarHeight(context: Context): Int {
        var result = 0
        val resourceId = context.resources.getIdentifier("status_bar_height", "dimen", "android")
        if (resourceId > 0) {
            result = context.resources.getDimensionPixelSize(resourceId)
        }
        return result
    }

    fun getNavigationBarHeight(context: Context): Int {
        val resources = context.resources
        val resourceId = resources.getIdentifier("navigation_bar_height", "dimen", "android")
        return if (resourceId > 0) resources.getDimensionPixelSize(resourceId) else 0
    }

    fun getTargetDisplayContext(context: Context, displayId: Int): Context {
        try {
            val displayManager:DisplayManager = context.getSystemService(DisplayManager::class.java)
            val targetDisplay = displayManager.getDisplay(displayId)
            val targetDisplayContext = context.createDisplayContext(targetDisplay)
            return targetDisplayContext
        } catch (e: Exception) {
            return context
        }
    }
}