package com.sgm.navi.ui.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatImageView;

import com.sgm.navi.ui.R;

/**
 * MFC态 ImageView
 * 使用：XML中需要配置 ：
 *   android:focusable="true"
 *   android:focusableInTouchMode="false"
 *   android:background="@drawable/bg_base_mfc_cadillac_bg_selector"
 *   图片资源使用SRC
 */

public class SkinImageView extends AppCompatImageView {
    private boolean mIsClickChangeColor;
    public SkinImageView(final Context context) {
        super(context);
    }

    public SkinImageView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
        initAttributes(context, attrs);
    }

    public SkinImageView(final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttributes(context, attrs);
    }

    /**
     * 初始化属性
     *
     * @param context 上下文
     * @param attrs   属性
     */
    private void initAttributes(final Context context, final AttributeSet attrs) {
        final TypedArray typedArray = context.obtainStyledAttributes(attrs,
                R.styleable.SkinClickChangeColor);
        mIsClickChangeColor = typedArray.getBoolean(
                R.styleable.SkinClickChangeColor_click_color_change, false);
        typedArray.recycle();
    }

    @Override
    protected void drawableStateChanged() {
        super.drawableStateChanged();
        boolean isPressed = isPressed();
        boolean isFocused = isFocused();
        if (isFocused && isPressed) {
            //MFC 获取焦点并且按压态 使用XML 配置MFC Background
        } else if (isFocused) {
            //MFC 获取焦点并且按压态  使用XML 配置MFC Background
        } else if (isPressed && mIsClickChangeColor) {
            // 触屏按压态
            setAlpha(0.6f);
        } else if (mIsClickChangeColor) {
            // 恢复默认透明度
            setAlpha(1.0f);
        }
    }
}
