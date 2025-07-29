package com.sgm.navi.ui.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;

import androidx.appcompat.widget.AppCompatButton;

import com.sgm.navi.ui.R;

public class SkinButton extends AppCompatButton {
    private boolean mIsClickChangeColor;
    public SkinButton(final Context context) {
        super(context);
    }

    public SkinButton(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinButton(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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
        } else if (mIsClickChangeColor){
            // 恢复默认透明度
            setAlpha(1.0f);
        }
    }
}
