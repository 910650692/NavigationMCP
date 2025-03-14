package com.android.utils

import android.annotation.SuppressLint
import android.content.Context
import android.os.Looper
import android.view.LayoutInflater
import android.view.View
import android.widget.TextView
import android.widget.Toast
import com.google.android.material.snackbar.Snackbar


/**
 * @Introduce: 吐司框管理.
 * @Author: lvww
 * @Date: 2023/11/13
 * @Description :后续待扩展
 */
class ToastUtils private constructor() {
    private var mToast: Toast? = null
    private var mContext: Context? = null
    private var myLopper: Looper? = null
    private var snackbar: Snackbar? = null

    fun init(context: Context) {
        mContext = context
    }

    @Deprecated(
        "兼容性低，多平台情况下容易出现显示不一致、遮挡、容易被攻击等问题",
        ReplaceWith("showCustomToastView"),
        level = DeprecationLevel.WARNING
    )
    fun showTextShort(sequence: CharSequence?) {
        showText(sequence, Toast.LENGTH_SHORT)
    }

    @Deprecated(
        "兼容性低，多平台情况下容易出现显示不一致、遮挡、容易被攻击等问题",
        ReplaceWith("showCustomToastView"),
        level = DeprecationLevel.WARNING
    )
    fun showTextLong(sequence: CharSequence?) {
        showText(sequence, Toast.LENGTH_LONG)
    }

    @Deprecated(
        "兼容性低，多平台情况下容易出现显示不一致、遮挡、容易被攻击等问题",
        ReplaceWith("showCustomToastView"),
        level = DeprecationLevel.WARNING
    )
    fun showText(sequence: CharSequence?, duration: Int) {
        showText(sequence, duration, true)
    }

    @Deprecated(
        "兼容性低，多平台情况下容易出现显示不一致、遮挡、容易被攻击等问题",
        ReplaceWith("showCustomToastView"),
        level = DeprecationLevel.WARNING
    )
    fun showText(sequence: CharSequence?, duration: Int, boolean: Boolean) {
        try {
            show(sequence, duration, boolean)
        } catch (e: Exception) {
            checkLooper(true)
            show(sequence, duration, true)
            checkLooper(false)
        }
    }

    @SuppressLint("InflateParams", "MissingInflatedId")
    fun showCustomToastView(msg: CharSequence) {
        showCustomToastView(msg.toString(), Toast.LENGTH_SHORT)
    }

    @SuppressLint("InflateParams")
    fun showCustomToastView(msg: String, time: Int) {
        cancelView()
        ConvertUtils.checkParam("show", msg, time)
        val view = LayoutInflater.from(mContext).inflate(R.layout.toast_base_view, null)
        var toastText = view.findViewById<TextView>(R.id.toast_text)
        toastText.text = msg
        mToast = Toast(mContext)
        mToast?.view = view
        mToast?.setDuration(time)
        mToast?.show()
    }

    fun showCustomToastView(view: View, msg: String) {
        cancelView()
        ConvertUtils.checkParam("show", view, msg)
        var toastText = view.findViewById<TextView>(R.id.toast_text)
        toastText.text = msg
        mToast = Toast(mContext)
        mToast?.view = view
        mToast?.setDuration(Toast.LENGTH_SHORT)
        mToast?.show()
    }

    /**
     * 展示土司.
     *
     * @param sequence
     * @param duration [ToastUtils]
     */
    private fun show(sequence: CharSequence?, duration: Int, cancel: Boolean) {
        ConvertUtils.checkParam("show", sequence)
        if (cancel) cancelView()
        mToast = Toast.makeText(mContext, sequence, duration)
        mToast?.show()
    }

    private fun cancelView() {
        mToast?.cancel()
        mToast = null
        snackbar?.dismiss()
        snackbar = null
    }

    fun destroy() {
        cancelView()
        mContext = null
        myLopper = null
    }

    private fun checkLooper(looperStatus: Boolean) {
        if (looperStatus) {
            if (null == myLopper) Looper.prepare()
        } else {
            if (null == myLopper) return
            Looper.loop()
            myLopper!!.quit()
        }
    }

    companion object {
        fun getInstance() = Helper.toast
    }

    object Helper {
        val toast = ToastUtils()
    }
}