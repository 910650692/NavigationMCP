package com.fy.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.SwitchCompat;

public class SkinSwitchCompat extends SwitchCompat {
    public SkinSwitchCompat(@NonNull final Context context) {
        this(context, null);
    }

    public SkinSwitchCompat(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinSwitchCompat(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }
}
