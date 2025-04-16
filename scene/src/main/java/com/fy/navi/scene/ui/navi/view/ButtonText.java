package com.fy.navi.scene.ui.navi.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.fy.navi.ui.view.SkinTextView;

/**
 * 实现按钮的点击效果的textview
 * @author fy
 * @version $Revision.*$
 */
public class ButtonText extends SkinTextView {

    public ButtonText(final Context context) {
        super(context);
    }

    public ButtonText(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public ButtonText(final Context context, @Nullable final AttributeSet attrs,
                      final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            if (isClickable()) {
                setAlpha(0.6f);
            }
            return true;
        } else if (event.getAction() == MotionEvent.ACTION_UP) {
            // 手指松开反馈点击事件并恢复控件原有的透明度
            if (isClickable()) {
                setAlpha(1.0f);
                callOnClick();
            }
            return true;
        }
        return super.onTouchEvent(event);
    }

}
