package com.sgm.navi.scene.ui.setting;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.LayerDrawable;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.widget.CompoundButton;
import android.widget.GridLayout;
import android.widget.LinearLayout;

import com.android.utils.ResourceUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.ui.view.SkinCheckBox;

import java.util.Arrays;
import java.util.List;

public class PlateNumberKeyboardView extends GridLayout {

    private static final String BUTTON_NAME = "删除";

    private static final String[] FIRST_ROW = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0"};
    private static final String[] SECOND_ROW = {"Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"};
    private static final String[] THIRD_ROW = {"A", "S", "D", "F", "G", "H", "J", "K", "L"};
    private static final String[] FOURTH_ROW = {"Z", "X", "C", "V", "B", "N", "M", BUTTON_NAME};

    private float mRowHeight = 0;
    private float mNumberRowWidth = 0;
    private float mThirdRowWidth = 0;
    private float mLastRowWidth = 0;
    private float mTextSize = 0;
    private float mInsertWidth = 0;
    private OnKeyPressListener mListener;
    private SkinCheckBox mLastSelectedButton;

    private static final List<String> DISABLED_KEYS = Arrays.asList("I", "O");


    public PlateNumberKeyboardView(final Context context) {
        this(context, null);
        init(context, null);
    }

    public PlateNumberKeyboardView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    /**
     * 初始化键盘布局
     * @param context 上下文
     * @param attrs 属性集合
     */
    private void init(final Context context, final AttributeSet attrs) {
        final TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.PlateNumberKeyboardView);
        mRowHeight = a.getDimension(R.styleable.PlateNumberKeyboardView_numberRowHeight, 0);
        mNumberRowWidth = a.getDimension(R.styleable.PlateNumberKeyboardView_numberRowWidth, 0);
        mThirdRowWidth = a.getDimension(R.styleable.PlateNumberKeyboardView_thirdRowWidth, 0);
        mLastRowWidth = a.getDimension(R.styleable.PlateNumberKeyboardView_lastRowWidth, 0);
        mTextSize = a.getFloat(R.styleable.PlateNumberKeyboardView_textSize, 0);
        mInsertWidth = a.getFloat(R.styleable.PlateNumberKeyboardView_insertWidth, 0);
        a.recycle();
        setOrientation(VERTICAL);
        // 添加四行按键
        addKeyboardRow(FIRST_ROW, true,false);
        addKeyboardRow(SECOND_ROW,true,false);
        addKeyboardRow(THIRD_ROW,false,true);
        addKeyboardRow(FOURTH_ROW,false,false);
    }

    /**
     * 添加键盘行
     * @param keys 按键数组
     * @param isNumberRow 是否为数字行
     * @param isThirdRow 是否为第三行
     */
    private void addKeyboardRow(final String[] keys, final boolean isNumberRow, final boolean isThirdRow) {
        final LinearLayout rowLayout = new LinearLayout(getContext());
        rowLayout.setOrientation(LinearLayout.HORIZONTAL);
        rowLayout.setLayoutParams(new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.WRAP_CONTENT
        ));
        float keyWidth = mLastRowWidth;
        final float keyHeight = mRowHeight;
        if (isNumberRow) {
            keyWidth = mNumberRowWidth;
        }
        if (isThirdRow) {
            keyWidth = mThirdRowWidth;
        }

        for (String key : keys) {
            final SkinCheckBox keyView = new SkinCheckBox(getContext());
            keyView.setText(key);
            keyView.setButtonDrawable(null);
            keyView.setTextSize(TypedValue.COMPLEX_UNIT_SP, mTextSize);
            keyView.setGravity(Gravity.CENTER);
            keyView.setFocusable(true);
            keyView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
            keyView.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
            if (BUTTON_NAME.equals(key)) {
                keyView.setText(null);
                final Drawable drawable = ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_plate_number_delete);
                final LayerDrawable layerDrawable = new LayerDrawable(new Drawable[]{drawable});
                layerDrawable.setLayerGravity(0, Gravity.CENTER);
                final int inset = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, mInsertWidth, getResources().getDisplayMetrics());
                layerDrawable.setLayerInset(0, inset, 0, 0, 0);
                keyView.setPadding(0, 0, 0, 0);
                keyView.setButtonDrawable(layerDrawable);

            }
            final LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(
                    (int) keyWidth,
                    (int) keyHeight
            );
            params.gravity = Gravity.CENTER;
            params.setMargins(0, 0, 8, 0);
            keyView.setLayoutParams(params);
            final boolean isDisabled = DISABLED_KEYS.contains(key);
            if (isDisabled) {
                keyView.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
                keyView.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
                keyView.setEnabled(false); // 禁用点击
                keyView.setAlpha(0.5f); // 设置透明度
            } else {
                // 设置点击事件
//                keyView.setOnClickListener(v -> {
//                    if (mListener != null) {
//                        if (BUTTON_NAME.equals(key)) {
//                            mListener.onDelete();
//                        } else {
//                            mListener.onKeyPress(key);
//                        }
//                        keyView.setSelected(true);
//                        updateCheckBoxTextColor(keyView,true);
//                        keyView.setBackgroundResource(R.drawable.bg_setting_keyboard_select_selector);
//                    }
//                });
                keyView.setOnTouchListener((v, event) -> {
                    switch (event.getAction()) {
                        case MotionEvent.ACTION_DOWN:
                            keyView.setSelected(true);
                            updateCheckBoxTextColor(keyView,true);
                            keyView.setBackgroundResource(R.drawable.bg_setting_keyboard_select_selector);
                            break;
                        case MotionEvent.ACTION_UP:
                            keyView.setSelected(false);
                            updateCheckBoxTextColor(keyView, false);
                            keyView.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
                            if (mListener != null) {
                                if (BUTTON_NAME.equals(key)) {
                                    mListener.onDelete();
                                } else {
                                    mListener.onKeyPress(key);
                                }
                            }
                            break;
                    }
                    return true;
                });
                keyView.setOnCheckedChangeListener((buttonView, isChecked) -> {
                    if (isChecked && (keyView != mLastSelectedButton)) {
                        if (mLastSelectedButton != null) {
                            mLastSelectedButton.setChecked(false);
                            mLastSelectedButton.setBackgroundResource(R.drawable.bg_setting_keyboard_selector);
                            updateCheckBoxTextColor(mLastSelectedButton,false);
                        }
                        mLastSelectedButton = keyView;
                    }
                });
            }
            rowLayout.addView(keyView);
        }
        final LayoutParams params = new LayoutParams();
        params.width = LayoutParams.MATCH_PARENT;
        params.height = LayoutParams.WRAP_CONTENT;
        params.setMargins(0, 0, 0, 8);
        rowLayout.setLayoutParams(params);
        addView(rowLayout);
    }

    /**
     * 更新CheckBox文本颜色
     * @param compoundButton CheckBox
     * @param isSelected 是否选中
     */
    public void updateCheckBoxTextColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
        } else {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_preference_text_gray));
        }
    }

    /**
     * 设置按键监听
     * @param listener 按键监听
     */
    public void setOnKeyPressListener(final OnKeyPressListener listener) {
        this.mListener = listener;
    }

    public interface OnKeyPressListener {

        /**
         * 按键按下回调
         * @param key 按键字符
         */
        void onKeyPress(String key);

        /**
         * 删除字符回调
         */
        void onDelete();
    }
}