package com.fy.navi.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SkinViewGroup extends ViewGroup {
    public SkinViewGroup(Context context) {
        super(context);
    }

    public SkinViewGroup(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinViewGroup(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {

    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    }

    @Override
    protected void onDraw(@NonNull Canvas canvas) {
        super.onDraw(canvas);
    }
}
