package com.fy.navi.hmi.setting.guide.platenumber;

import android.text.TextUtils;
import android.view.View;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingPlateNumberBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.scene.ui.setting.PlateNumberKeyboardView;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.Objects;

public class SettingPlateNumberFragment extends BaseFragment<FragmentSettingPlateNumberBinding, SettingPlateNumberViewModel> {

    private SettingCheckDialog deletePlateNumberDialog;

    public boolean isClearPlateNumber = false;

    public String plateNumber = "";

    public static final String CARMATCH = "^[京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽赣贵粤青藏川宁琼使领][A-Z][A-Z0-9]{4}[A-Z0-9挂学警港澳]$";
    public static final String EVCARMATCH = "^[京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽贵粤青藏川宁琼使领A-Z]{1}[A-Z]{1}(([0-9]{5}[A-HJ-K]$)|([A-HJ-K][A-HJ-NP-Z0-9][0-9]{4}$))";
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_plate_number;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {

        mBinding.settingPlateNumberNumber.setShowSoftInputOnFocus(false);
        mBinding.settingPlateNumberFinish.setEnabled(false);

        mBinding.settingPlateNumberNumber.setOnFocusChangeListener((v, hasFocus) -> {
            if (hasFocus) {
                showPlateNumberKeyboard();
                mViewModel.setIsFocus(true);
            }
        });

        mBinding.settingProvinceKeyboard.setOnProvinceSelectedListener(province -> {
            mBinding.settingPlateNumberProvince.setText(province);
        });

        mBinding.settingPlateNumberKeyboard.setOnKeyPressListener(new PlateNumberKeyboardView.OnKeyPressListener() {

            @Override
            public void onKeyPress(String key) {
                int start = mBinding.settingPlateNumberNumber.getSelectionStart();
                String text = mBinding.settingPlateNumberNumber.getText().toString();

                if (!isEVCar() && text.length() >= 5) {
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                } else if (isEVCar() && text.length() >= 6){
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                } else {
                    mBinding.settingPlateNumberFinish.setEnabled(false);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_gray));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_normal));
                }

                // 第一位只能输入字母
                if (text.isEmpty()) {
                    if (key.matches("[A-Z]")) {  // 只允许输入字母
                        mBinding.settingPlateNumberNumber.getText().insert(start, key);
                    }
                    return;
                }

                // 其他位置的输入限制
                // 限制最大输入长度
                if (!isEVCar() && text.length() < 6) {
                    mBinding.settingPlateNumberNumber.getText().insert(start, key);
                } else if (isEVCar() && text.length() < 7) {
                    mBinding.settingPlateNumberNumber.getText().insert(start, key);
                }

            }

            @Override
            public void onDelete() {
                int start = mBinding.settingPlateNumberNumber.getSelectionStart();
                int end = mBinding.settingPlateNumberNumber.getSelectionEnd();

                if (start == end && start > 0) {
                    // 没有选中文本，删除光标前一个字符
                    mBinding.settingPlateNumberNumber.getText().delete(start - 1, start);
                } else if (start < end) {
                    // 删除选中的文本
                    mBinding.settingPlateNumberNumber.getText().delete(start, end);
                }
                String text = mBinding.settingPlateNumberNumber.getText().toString();
                if (!isEVCar() && text.length() >= 6) {
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                } else if (isEVCar() && text.length() >= 7){
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                } else {
                    mBinding.settingPlateNumberFinish.setEnabled(false);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_gray));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_normal));
                }
                if (text.isEmpty() && !Objects.equals(plateNumber, "")) {
                    isClearPlateNumber = true;
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
                    mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                }
            }
        });
        setPlateNumber();
        if (!isEVCar()) {
            mBinding.settingCarTypeDetail.setText(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_car_type_oil));
        } else {
            mBinding.settingCarTypeDetail.setText(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_car_type_ev));
        }
        initDialog();
    }

    @Override
    public void onInitData() {

    }

    public void initDialog() {
        deletePlateNumberDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_clear))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_clear_tip))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.favorite_item_delete))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        isClearPlateNumber = false;
                        SettingUpdateObservable.getInstance().setPlateNumber("");
                        closeFragment(true);
                    }

                }).build();

        clearBackground(deletePlateNumberDialog.getWindow());
    }

    private void clearBackground(Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    public void showProvinceKeyboard(){
        mBinding.settingPlateNumberKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.VISIBLE);
        mBinding.settingProvinceKeyboard.setVisibility(View.VISIBLE);
    }

    public void showPlateNumberKeyboard() {
        mBinding.settingProvinceKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.VISIBLE);
        mBinding.settingPlateNumberKeyboard.setVisibility(View.VISIBLE);
    }

    public void hideKeyboard(){
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.GONE);
        mBinding.settingProvinceKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboard.setVisibility(View.GONE);
    }

    private boolean isCar(String carNumber) {
        if (TextUtils.isEmpty(carNumber)) {
            return false;
        } else if (isEVCar()){
            return carNumber.matches(EVCARMATCH);
        } else {
            return carNumber.matches(CARMATCH);
        }
    }

    public void plateNumberInputFinish() {

        Logger.d("plateNumberInputFinish");

        String plateNumber = mBinding.settingPlateNumberProvince.getText().toString() + mBinding.settingPlateNumberNumber.getText().toString();
        if (isCar(plateNumber)) {
            SettingUpdateObservable.getInstance().setPlateNumber(plateNumber);
            closeFragment(true);
        } else if (isClearPlateNumber){
            deletePlateNumberDialog.show();
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("车牌校验未通过");
        }
    }

    private void setPlateNumber(){
        String plateNumber = mViewModel.getPlateNumber();
        this.plateNumber = plateNumber;
        if (plateNumber.isEmpty()) {
            return;
        }
        mBinding.settingPlateNumberFinish.setEnabled(true);
        mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
        mBinding.settingPlateNumberFinish.setBackground(ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
        mBinding.settingPlateNumberProvince.setText(plateNumber.substring(0, 1));
        mBinding.settingPlateNumberNumber.setText(plateNumber.substring(1));
    }
    public void clearPlateNumber(){
        if (!Objects.equals(plateNumber, "")) {
            isClearPlateNumber = true;
        }
        mBinding.settingPlateNumberProvince.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_guide_plate_number_default));
        mBinding.settingPlateNumberNumber.setText("");
    }

    public boolean isEVCar() {
        int carMode = CalibrationPackage.getInstance().powerType();
        return carMode == 1;
    }
}
