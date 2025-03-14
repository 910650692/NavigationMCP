package com.fy.navi.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SkinLinearLayout extends LinearLayoutCompat {
    public SkinLinearLayout(Context context) {
        super(context);
    }

    public SkinLinearLayout(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinLinearLayout(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        super.onLayout(changed, l, t, r, b);
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
    }
}
