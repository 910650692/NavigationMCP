package com.fy.navi.scene.ui.navi.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.fy.navi.ui.view.SkinTextView;

/**
 * 实现按钮的点击效果的textview
 */
public class ButtonText extends SkinTextView {

    public ButtonText(Context context) {
        super(context);
    }

    public ButtonText(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ButtonText(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            setAlpha(0.6f);
            return true;
        } else if (event.getAction() == MotionEvent.ACTION_UP) {
            // 手指松开反馈点击事件并恢复控件原有的透明度
            setAlpha(1.0f);
            callOnClick();
            return true;
        }
        return super.onTouchEvent(event);
    }

}
