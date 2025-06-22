package com.sgm.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.tabs.TabLayout;

public class SkinTabLayout extends TabLayout {
    public SkinTabLayout(@NonNull final Context context) {
        super(context);
    }

    public SkinTabLayout(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinTabLayout(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }
}
