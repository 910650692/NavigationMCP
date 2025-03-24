package com.fy.navi.scene.ui.navi.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;

import androidx.annotation.Nullable;

import com.fy.navi.ui.view.SkinImageView;

/**
 * 实现点击效果的ImageView
 * @author fy
 * @version $Revision.*$
 */
public class ButtonImageView extends SkinImageView {

    public ButtonImageView(final Context context) {
        super(context);
    }

    public ButtonImageView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public ButtonImageView(final Context context,
                           @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(final MotionEvent event) {
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
