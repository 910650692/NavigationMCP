package com.sgm.navi.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;

public class SkinLinearLayout extends LinearLayoutCompat {
    public SkinLinearLayout(final Context context) {
        super(context);
    }

    public SkinLinearLayout(final Context context, final @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinLinearLayout(final Context context, final @Nullable AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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
}
