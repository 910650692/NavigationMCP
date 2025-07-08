package com.sgm.navi.scene.ui.setting;


import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.SoundEffectConstants;
import android.widget.CompoundButton;
import android.widget.GridLayout;

import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.ui.view.SkinCheckBox;

public class ProvinceKeyboardView extends GridLayout {
    private static final String[] PROVINCES = {"京", "津", "冀", "晋", "蒙", "辽", "吉", "黑", "沪",
            "苏", "浙", "皖", "闽", "赣", "鲁", "豫", "鄂", "湘", "粤", "桂", "琼", "渝", "川", "贵",
            "云", "藏", "陕", "甘", "青", "宁", "新"};

    private OnProvinceSelectedListener mListener;
    private SkinCheckBox mLastSelectedButton;

    public ProvinceKeyboardView(final Context context) {
        this(context, null);
        init(context, null);
    }

    public ProvinceKeyboardView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    /**
     * 初始化
     * @param context 上下文
     * @param attrs 属性
     */
    private void init(final Context context, final AttributeSet attrs) {
        setColumnCount(8);
        setRowCount(4);

        final TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.ProvinceKeyboardView);
        final float textSize = a.getFloat(R.styleable.ProvinceKeyboardView_provinceTextSize, 0);
        final float width = a.getDimension(R.styleable.ProvinceKeyboardView_rowWidth, 0);
        final float height = a.getDimension(R.styleable.ProvinceKeyboardView_rowHeight, 0);
        for (String province : PROVINCES) {
            final SkinCheckBox tv = new SkinCheckBox(getContext());
            tv.setText(province);
            tv.setTextSize(TypedValue.COMPLEX_UNIT_SP, textSize);
            tv.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
            tv.setGravity(Gravity.CENTER);
            tv.setButtonDrawable(null);
            tv.setFocusable(true);
            tv.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
            tv.setOnTouchListener((v, event) -> {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        v.playSoundEffect(SoundEffectConstants.CLICK);
                        break;
                    case MotionEvent.ACTION_UP:
                        tv.setSelected(true);
                        updateCheckBoxTextColor(tv, true);
                        tv.setBackgroundResource(R.drawable.bg_setting_keyboard_select_selector);
                        if (mListener != null) {
                            mListener.onProvinceSelected(province);
                        }
                        tv.setChecked(true);
                        break;
                }
                return true;
            });

            tv.setOnCheckedChangeListener((buttonView, isChecked) -> {
                if (isChecked && (tv != mLastSelectedButton)) {
                    if (mLastSelectedButton != null) {
                        mLastSelectedButton.setChecked(false);
                        mLastSelectedButton.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
                        updateCheckBoxTextColor(mLastSelectedButton,false);
                    }
                    mLastSelectedButton = tv;
                }
            });

            final LayoutParams params = new LayoutParams();
            params.width = (int)width;
            params.height = (int)height;
            params.setMargins(0, 0, 8, 8);
            addView(tv, params);
        }
    }

    /**
     * 更新CheckBox文本颜色
     * @param compoundButton CheckBox
     * @param isSelected 是否选中
     */
    public void updateCheckBoxTextColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(R.color.setting_white));
        } else {
            compoundButton.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
        }
    }

    /**
     * 设置初始选中的省份
     * @param province 省份
     */
    public void setSelectedProvince(final String province) {
        for (int i = 0; i < getChildCount(); i++) {
            final SkinCheckBox checkBox = (SkinCheckBox) getChildAt(i);
            if (checkBox.getText().toString().equals(province)) {
                checkBox.setChecked(true);
                updateCheckBoxTextColor(checkBox, true);
                checkBox.setBackgroundResource(R.drawable.bg_setting_keyboard_select_selector);
                mLastSelectedButton = checkBox;
            } else {
                checkBox.setChecked(false);
                updateCheckBoxTextColor(checkBox, false);
                checkBox.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
            }
        }
    }

    /**
     * 设置按键监听
     * @param listener 按键监听
     */
    public void setOnProvinceSelectedListener(final OnProvinceSelectedListener listener) {
        this.mListener = listener;
    }

    public interface OnProvinceSelectedListener {

        /**
         * 省份选择
         * @param province 省份
         */
        void onProvinceSelected(String province);
    }
}