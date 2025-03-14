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

    private OnProvinceSelectedListener listener;
    private SkinCheckBox lastSelectedButton;

    public ProvinceKeyboardView(Context context) {
        this(context, null);
    }

    public ProvinceKeyboardView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setColumnCount(8);
        setRowCount(4);
        for (String province : PROVINCES) {
            SkinCheckBox tv = new SkinCheckBox(getContext());
            tv.setText(province);
            tv.setTextSize(TypedValue.COMPLEX_UNIT_SP, 30);
            tv.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
            tv.setGravity(Gravity.CENTER);
            tv.setButtonDrawable(null);
            tv.setBackgroundResource(R.drawable.bg_setting_preference_normal);
            tv.setOnClickListener(v -> {
                if (listener != null) {
                    tv.setSelected(true);
                    updateCheckBoxTextColor(tv,true);
                    tv.setBackgroundResource(R.drawable.bg_setting_preference_select);
                    listener.onProvinceSelected(province);
                }
            });

            tv.setOnCheckedChangeListener((buttonView, isChecked) -> {
                if (isChecked && (tv != lastSelectedButton)) {
                    if (lastSelectedButton != null) {
                        lastSelectedButton.setChecked(false);
                        lastSelectedButton.setBackgroundResource(R.drawable.bg_setting_preference_normal);
                        updateCheckBoxTextColor(lastSelectedButton,false);
                    }
                    lastSelectedButton = tv;
                }
            });

            LayoutParams params = new LayoutParams();
            params.width = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_136);
            params.height = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_76);
            params.setMargins(0, 0, 8, 8);
            addView(tv, params);
        }
    }

    public void updateCheckBoxTextColor(CompoundButton compoundButton, boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(R.color.white));
        } else {
            compoundButton.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
        }
    }

    public void setOnProvinceSelectedListener(OnProvinceSelectedListener listener) {
        this.listener = listener;
    }

    public interface OnProvinceSelectedListener {
        void onProvinceSelected(String province);
    }
}