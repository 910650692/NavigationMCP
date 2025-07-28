package com.sgm.navi.ui.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;

import com.sgm.navi.ui.R;

public class SkinLinearLayout extends LinearLayoutCompat {

    private boolean mIsClickChangeColor;
    public SkinLinearLayout(final Context context) {
        super(context);
    }

    public SkinLinearLayout(final Context context, final @Nullable AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    public SkinLinearLayout(final Context context, final @Nullable AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs);
    }

    private void init(Context context, AttributeSet attrs) {
        final TypedArray typedArray = context.obtainStyledAttributes(attrs,
                R.styleable.SkinClickChangeColor);
        mIsClickChangeColor = typedArray.getBoolean(
                R.styleable.SkinClickChangeColor_click_color_change, false);
        typedArray.recycle();
    }

    @Override
    protected void onLayout(final boolean changed, final int l, final int t, final int r, final int b) {
        super.onLayout(changed, l, t, r, b);
    }

    @Override
    protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }

    @Override
    protected void onDraw(final Canvas canvas) {
        super.onDraw(canvas);
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
