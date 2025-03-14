package com.fy.navi.ui.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.MotionEvent;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.log.Logger;
import com.fy.navi.ui.R;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SkinConstraintLayout extends ConstraintLayout {

    /**
     * true：点击设置60%不透明度，抬起恢复原状（按压态）
     */
    private boolean mIsClickChangeColor = false;

    public SkinConstraintLayout(@NonNull Context context) {
        this(context, null);
    }

    public SkinConstraintLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinConstraintLayout(@NonNull Context context, @Nullable AttributeSet attrs,
                                int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttributes(context, attrs);
    }

    private void initAttributes(Context context, AttributeSet attrs) {
        @SuppressLint("Recycle") TypedArray typedArray = context.obtainStyledAttributes(attrs,
                R.styleable.SkinConstraintLayout);
        mIsClickChangeColor = typedArray.getBoolean(
                R.styleable.SkinConstraintLayout_click_color_change, false);
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN && mIsClickChangeColor) {
            setAlpha(0.6f);
            return true;
        } else if (event.getAction() == MotionEvent.ACTION_UP && mIsClickChangeColor) {
            // 手指松开反馈点击事件并恢复控件原有的透明度
            setAlpha(1.0f);
            callOnClick();
            return true;
        }
        return super.onTouchEvent(event);
    }
}
