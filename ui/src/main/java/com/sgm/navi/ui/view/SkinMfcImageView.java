package com.sgm.navi.ui.view;

import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.InputDevice;
import android.view.MotionEvent;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.core.view.ViewCompat;

import com.android.utils.log.Logger;

///**
// * MFC态 ImageView
// * 使用：XML中需要配置 ：
// *   android:focusable="true"
// *   android:focusableInTouchMode="false"
// *   android:background="@drawable/bg_base_mfc_cadillac_bg_selector"
// *   图片资源使用SRC
// */
public class SkinMfcImageView extends AppCompatImageView {
    public SkinMfcImageView(final Context context) {
        super(context);
    }

    public SkinMfcImageView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinMfcImageView(final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void drawableStateChanged() {
        super.drawableStateChanged();
        boolean isPressed = isPressed();
        boolean isFocused = isFocused();
        Logger.d("testTAG", "drawableStateChanged: " + isPressed + ", " + isFocused);
        if (isFocused && isPressed) {
            //MFC 获取焦点并且按压态 使用XML 配置MFC Background
        } else if (isFocused) {
            //MFC 获取焦点并且按压态  使用XML 配置MFC Background
        } else if (isPressed) {
            // 触屏按压态
            setAlpha(0.6f);
        } else {
            // 恢复默认透明度
            setAlpha(1.0f);
        }
    }
}
