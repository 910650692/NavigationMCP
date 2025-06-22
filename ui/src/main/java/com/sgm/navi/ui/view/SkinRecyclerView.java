package com.sgm.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;

public class SkinRecyclerView extends RecyclerView {
    public SkinRecyclerView(@NonNull final Context context) {
        super(context);
    }

    public SkinRecyclerView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinRecyclerView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }
}
