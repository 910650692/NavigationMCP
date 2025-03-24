package com.fy.navi.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

public class SkinViewGroup extends ViewGroup {
    public SkinViewGroup(final Context context) {
        super(context);
    }

    public SkinViewGroup(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinViewGroup(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onLayout(final boolean changed, final int l, final int t, final int r, final int b) {

    }

    @Override
    protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }

    @Override
    protected void onDraw(@NonNull final Canvas canvas) {
        super.onDraw(canvas);
    }
}
