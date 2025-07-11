package com.sgm.navi.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;

public class SkinRecyclerView extends RecyclerView {

    private OnAnyTouchListener onAnyTouchListener;

    public void setOnAnyTouchListener(OnAnyTouchListener listener) {
        this.onAnyTouchListener = listener;
    }

    public void removeOnAnyTouchListener() {
        this.onAnyTouchListener = null;
    }

    public interface OnAnyTouchListener {
        void onAnyTouch(MotionEvent event);
    }

    public SkinRecyclerView(@NonNull final Context context) {
        super(context);
    }

    public SkinRecyclerView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SkinRecyclerView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public boolean dispatchTouchEvent(MotionEvent ev) {
        if (onAnyTouchListener != null) {
            onAnyTouchListener.onAnyTouch(ev);
        }
        return super.dispatchTouchEvent(ev);
    }
}
