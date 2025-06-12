package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.util.AttributeSet;

import com.fy.navi.ui.view.SkinCheckBox;

public class DynamicCheckBox extends SkinCheckBox {

    private boolean mIsChecked = false;

    public DynamicCheckBox(Context context) {
        super(context);
    }

    public DynamicCheckBox(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public DynamicCheckBox(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        setOnClickListener(v -> {
            if (onCheckedChangeListener != null) {
                onCheckedChangeListener.onCheckedChanged(DynamicCheckBox.this, mIsChecked);
            }
        });
    }

    @Override
    public void setChecked(boolean checked) {
        if (mIsChecked == checked) return;
        mIsChecked = checked;
        refreshDrawableState();
        jumpDrawablesToCurrentState();
    }

    @Override
    public boolean isChecked() {
        return mIsChecked;
    }

    @Override
    protected int[] onCreateDrawableState(int extraSpace) {
        int[] drawableState = super.onCreateDrawableState(extraSpace + 1);
        if (isChecked()) {
            mergeDrawableStates(drawableState, new int[]{android.R.attr.state_checked});
        }
        return drawableState;
    }

    @Override
    public void toggle() {

    }

    private OnCheckedChangeListener onCheckedChangeListener;

    public void setOnCheckedChangeListener(OnCheckedChangeListener listener) {
        this.onCheckedChangeListener = listener;
    }

    public interface OnCheckedChangeListener {
        void onCheckedChanged(DynamicCheckBox checkBox, boolean isChecked);
    }
}
