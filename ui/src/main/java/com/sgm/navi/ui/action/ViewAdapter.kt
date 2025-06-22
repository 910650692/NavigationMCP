package com.sgm.navi.ui.action

import android.annotation.SuppressLint
import android.graphics.Bitmap
import android.graphics.drawable.Drawable
import android.view.View
import android.widget.EditText
import android.widget.ImageView
import androidx.databinding.BindingAdapter
import com.bumptech.glide.Glide
import com.bumptech.glide.load.DataSource
import com.bumptech.glide.load.engine.GlideException
import com.bumptech.glide.request.RequestListener
import com.bumptech.glide.request.target.Target
import com.jakewharton.rxbinding2.view.RxView
import com.jakewharton.rxbinding2.widget.RxTextView
import java.util.Objects
import java.util.concurrent.TimeUnit

@SuppressLint("CheckResult")
@BindingAdapter("clickCommand")
fun clickCommand(view: View, action: Action) {
    RxView.clicks(view).throttleFirst(500, TimeUnit.MILLISECONDS).subscribe { action.call() }
}

/**
 * 点击事件,可返回Tag值.
 */
@SuppressLint("CheckResult")
@BindingAdapter("clickCommandTag")
fun <T> clickCommand(view: View, action: Action1<T>) {
    RxView.clicks(view).throttleFirst(500, TimeUnit.MILLISECONDS).subscribe {
        val obj = view.tag
        if (Objects.nonNull(obj)) action.call(obj as T)
    }
}

/**
 * 点击事件,可返回自定义属性param的值.
 */
@SuppressLint("CheckResult")
@BindingAdapter("clickCommandParam", "param")
fun <P> clickCommand(view: View, action: Action2<P>, param: Any) {
    RxView.clicks(view).throttleFirst(500, TimeUnit.MILLISECONDS).map { param as P }
        .subscribe { p: P -> action.call(p) }
}

/**
 * 点击事件,可返回Tag和自定义属性Param的值.
 */
@SuppressLint("CheckResult")
@BindingAdapter("clickCommandTagParam", "param")
fun <T, P> clickCommand(view: View, action: Action3<T, P>, param: Any) {
    RxView.clicks(view).throttleFirst(500, TimeUnit.MILLISECONDS).subscribe {
        val obj = view.tag
        if (Objects.nonNull(obj) && Objects.nonNull(param)) action.call(
            obj as T,
            param as P
        )
    }
}

/**
 * 长按点击事件.
 */
@SuppressLint("CheckResult")
@BindingAdapter("onLongClickCommand")
fun onLongClickCommand(view: View, action: Action) {
    RxView.longClicks(view).throttleFirst(500, TimeUnit.MILLISECONDS)
        .subscribe { action.call() }
}

@SuppressLint("CheckResult")
@BindingAdapter("textWatcher")
fun textChange(editText: EditText, action: Action1<String>) {
    RxTextView.textChanges(editText).subscribe { t: CharSequence? -> action.call(t.toString()) }
}

@BindingAdapter("imageRes")
fun loadImage(imageView: ImageView, resId: Int) {
    imageView.setImageResource(resId)
}

@BindingAdapter("imageRes")
fun loadImage(imageView: ImageView, bitmap: Bitmap?) {
    if (null != bitmap) imageView.setImageBitmap(bitmap)
}

@BindingAdapter("imageRes")
fun loadImage(imageView: ImageView, drawable: Drawable?) {
    if (null != drawable) imageView.setImageDrawable(drawable)
}

@BindingAdapter(value = ["img_url", "placeHolder", "error"], requireAll = false)
fun loadImageUrl(imgView: ImageView, imgUrl: String, placeHolder: Int, error: Int) {
    Glide.with(imgView.context).load(imgUrl).placeholder(placeHolder).error(error).into(imgView)
}

fun loadImageUrl(imgView: ImageView, imgUrl: String, onImageLoadListener: OnImageLoadListener) {
    Glide.with(imgView.context).load(imgUrl).addListener(
        object : RequestListener<Drawable> {
            override fun onLoadFailed(e: GlideException?, model: Any?, target: Target<Drawable>, isFirstResource: Boolean): Boolean {
                onImageLoadListener.onLoadCompleted(false)
                return false
            }

            override fun onResourceReady(
                resource: Drawable,
                model: Any,
                target: Target<Drawable>?,
                dataSource: DataSource,
                isFirstResource: Boolean
            ): Boolean {
                onImageLoadListener.onLoadCompleted(true)
                return false
            }
        }
    ).into(imgView)
}