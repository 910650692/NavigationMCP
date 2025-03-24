package com.fy.navi.scene.ui.setting;


import android.content.Context;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;
import android.widget.CompoundButton;
import android.widget.GridLayout;

import com.android.utils.ResourceUtils;
import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinCheckBox;

public class ProvinceKeyboardView extends GridLayout {
    private static final String[] PROVINCES = {"京", "津", "冀", "晋", "蒙", "辽", "吉", "黑", "沪",
            "苏", "浙", "皖", "闽", "赣", "鲁", "豫", "鄂", "湘", "粤", "桂", "琼", "渝", "川", "贵",
            "云", "藏", "陕", "甘", "青", "宁", "新"};

    private OnProvinceSelectedListener mListener;
    private SkinCheckBox mLastSelectedButton;

    public ProvinceKeyboardView(final Context context) {
        this(context, null);
    }

    public ProvinceKeyboardView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    /**
     * 初始化
     */
    private void init() {
        setColumnCount(8);
        setRowCount(4);
        for (String province : PROVINCES) {
            final SkinCheckBox tv = new SkinCheckBox(getContext());
            tv.setText(province);
            tv.setTextSize(TypedValue.COMPLEX_UNIT_SP, 30);
            tv.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
            tv.setGravity(Gravity.CENTER);
            tv.setButtonDrawable(null);
            tv.setBackgroundResource(R.drawable.bg_setting_preference_normal);
            tv.setOnClickListener(v -> {
                if (mListener != null) {
                    tv.setSelected(true);
                    updateCheckBoxTextColor(tv,true);
                    tv.setBackgroundResource(R.drawable.bg_setting_preference_select);
                    mListener.onProvinceSelected(province);
                }
            });

            tv.setOnCheckedChangeListener((buttonView, isChecked) -> {
                if (isChecked && (tv != mLastSelectedButton)) {
                    if (mLastSelectedButton != null) {
                        mLastSelectedButton.setChecked(false);
                        mLastSelectedButton.setBackgroundResource(R.drawable.bg_setting_preference_normal);
                        updateCheckBoxTextColor(mLastSelectedButton,false);
                    }
                    mLastSelectedButton = tv;
                }
            });

            final LayoutParams params = new LayoutParams();
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_136);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_76);
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
            compoundButton.setTextColor(getResources().getColor(R.color.white));
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
                checkBox.setBackgroundResource(R.drawable.bg_setting_preference_select);
                mLastSelectedButton = checkBox;
            } else {
                checkBox.setChecked(false);
                updateCheckBoxTextColor(checkBox, false);
                checkBox.setBackgroundResource(R.drawable.bg_setting_preference_normal);
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