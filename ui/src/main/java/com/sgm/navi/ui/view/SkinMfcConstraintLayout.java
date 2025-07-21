package com.sgm.navi.ui.view;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.SoundEffectConstants;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.sgm.navi.ui.R;

/**
 * MFC态 ConstraintLayout
 * 使用：XML中需要配置 ：
 *   android:focusable="true"
 *   android:focusableInTouchMode="false"
 *   android:background="@drawable/bg_base_mfc_cadillac_bg_selector"
 */

public class SkinMfcConstraintLayout extends ConstraintLayout {


    public SkinMfcConstraintLayout(final @NonNull Context context) {
        this(context, null);
    }

    public SkinMfcConstraintLayout(final @NonNull Context context, final @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinMfcConstraintLayout(final @NonNull Context context, final @Nullable AttributeSet attrs,
                                   final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
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
        } else if (isPressed) {
            // 触屏按压态
            setAlpha(0.6f);
        } else {
            // 恢复默认透明度
            setAlpha(1.0f);
        }
    }


}
