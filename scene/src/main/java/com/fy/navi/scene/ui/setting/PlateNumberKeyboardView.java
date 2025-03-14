package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.LayerDrawable;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.ViewGroup;
import android.widget.CompoundButton;
import android.widget.GridLayout;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.android.utils.ResourceUtils;
import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinCheckBox;

import java.util.Arrays;
import java.util.List;

public class PlateNumberKeyboardView extends GridLayout {
    private static final String[] FIRST_ROW = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0"};
    private static final String[] SECOND_ROW = {"Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"};
    private static final String[] THIRD_ROW = {"A", "S", "D", "F", "G", "H", "J", "K", "L"};
    private static final String[] FOURTH_ROW = {"Z", "X", "C", "V", "B", "N", "M", "删除"};

    private OnKeyPressListener listener;
    private SkinCheckBox lastSelectedButton;

    private static final List<String> DISABLED_KEYS = Arrays.asList("I", "O");


    public PlateNumberKeyboardView(Context context) {
        this(context, null);
    }

    public PlateNumberKeyboardView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    private void init() {
        setOrientation(VERTICAL);
        // 添加四行按键
        addKeyboardRow(FIRST_ROW, true,false);
        addKeyboardRow(SECOND_ROW,true,false);
        addKeyboardRow(THIRD_ROW,false,true);
        addKeyboardRow(FOURTH_ROW,false,false);
    }

    private void addKeyboardRow(String[] keys, boolean isNumberRow, boolean isThirdRow) {
        LinearLayout rowLayout = new LinearLayout(getContext());
        rowLayout.setOrientation(LinearLayout.HORIZONTAL);

        rowLayout.setLayoutParams(new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.WRAP_CONTENT
        ));

        int keyWidth = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_136);
        int keyHeight = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_76);

        if (isNumberRow) {
            keyWidth = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_107);
        }
        if (isThirdRow) {
            keyWidth = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_120);
        }

        for (String key : keys) {
            SkinCheckBox keyView = new SkinCheckBox(getContext());
            keyView.setText(key);
            keyView.setButtonDrawable(null);
            keyView.setTextSize(TypedValue.COMPLEX_UNIT_SP, 30);
            keyView.setGravity(Gravity.CENTER);
            keyView.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
            keyView.setBackgroundResource(R.drawable.bg_setting_preference_normal);
            if ("删除".equals(key)) {

                keyView.setText(null);

                Drawable drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_plate_number_delete);
                LayerDrawable layerDrawable = new LayerDrawable(new Drawable[]{drawable});
                layerDrawable.setLayerGravity(0, Gravity.CENTER);
                int inset = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 40, getResources().getDisplayMetrics());
                layerDrawable.setLayerInset(0, inset, inset, inset, inset);

                keyView.setPadding(0, 0, 0, 0);
                keyView.setButtonDrawable(layerDrawable);

            }


            LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                    keyWidth,
                    keyHeight
            );

            params.gravity = Gravity.CENTER;
            params.setMargins(0, 0, 8, 0);
            keyView.setLayoutParams(params);

            boolean isDisabled = DISABLED_KEYS.contains(key);
            if (isDisabled) {
                // 设置禁用状态的样式
                keyView.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
                keyView.setBackgroundResource(R.drawable.bg_car_number_disable);
                keyView.setEnabled(false); // 禁用点击
            } else {
                // 设置点击事件
                keyView.setOnClickListener(v -> {
                    if (listener != null) {
                        if ("删除".equals(key)) {
                            listener.onDelete();
                        } else {
                            listener.onKeyPress(key);
                        }
                        keyView.setSelected(true);
                        updateCheckBoxTextColor(keyView,true);
                        keyView.setBackgroundResource(R.drawable.bg_setting_preference_select);
                    }
                });

                keyView.setOnCheckedChangeListener((buttonView, isChecked) -> {
                    if (isChecked && (keyView != lastSelectedButton)) {
                        if (lastSelectedButton != null) {
                            lastSelectedButton.setChecked(false);
                            lastSelectedButton.setBackgroundResource(R.drawable.bg_setting_preference_normal);
                            updateCheckBoxTextColor(lastSelectedButton,false);
                        }
                        lastSelectedButton = keyView;
                    }
                });

            }
            rowLayout.addView(keyView);
        }
        LayoutParams params = new LayoutParams();
        params.width = LayoutParams.MATCH_PARENT;
        params.height = LayoutParams.WRAP_CONTENT;
        params.setMargins(0, 0, 0, 8);
        rowLayout.setLayoutParams(params);
        addView(rowLayout);
    }

    public void updateCheckBoxTextColor(CompoundButton compoundButton, boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(R.color.white));
        } else {
            compoundButton.setTextColor(getResources().getColor(R.color.setting_preference_text_gray));
        }
    }

    public void setOnKeyPressListener(OnKeyPressListener listener) {
        this.listener = listener;
    }

    public interface OnKeyPressListener {
        void onKeyPress(String key);
        void onDelete();
    }
}