package com.fy.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.SwitchCompat;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/17
 */
public class SkinSwitchCompat extends SwitchCompat {
    public SkinSwitchCompat(@NonNull Context context) {
        this(context, null);
    }

    public SkinSwitchCompat(@NonNull Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinSwitchCompat(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }
}
