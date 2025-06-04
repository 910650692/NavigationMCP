package com.fy.navi.hmi.setting.guide.platenumber;

import android.text.TextUtils;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

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

    private SettingCheckDialog mDeletePlateNumberDialog;

    private boolean mIsClearPlateNumber = false;

    private String mPlateNumber = "";

    public static final String CARMATCH =
            "^[京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽赣贵粤青藏川宁琼使领][A-Z][A-Z0-9]{4}[A-Z0-9挂学警港澳]$";
    public static final String EVCARMATCH =
            "^[京津沪渝冀豫云辽黑湘皖鲁新苏浙赣鄂桂甘晋蒙陕吉闽赣贵粤青藏川宁琼使领A-Z]{1}[A-Z]{1}(([0-9]{5}[A-HJ-K]$)|([A-HJ-K][A-HJ-NP-Z0-9][0-9]{4}$))";
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_plate_number;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onResume() {
        super.onResume();
        hideSoftKeyboard();
    }

    @Override
    public void onInitView() {
        mBinding.settingPlateNumberNumber.setShowSoftInputOnFocus(false);
        mBinding.settingPlateNumberProvince.setText(mViewModel.getProvince().isEmpty() ?
                ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_default) :
                mViewModel.getProvince());
        mBinding.settingPlateNumberNumber.setText(mViewModel.getPlateNumberString());
        setFinishButtonState(mViewModel.getPlateNumberString());
        mBinding.settingPlateNumberNumber.setOnFocusChangeListener((v, hasFocus) -> {
            if (hasFocus) {
                showPlateNumberKeyboard();
            }
        });
        mBinding.settingProvinceKeyboard.setOnProvinceSelectedListener(province -> {
            mBinding.settingPlateNumberProvince.setText(province);
            mViewModel.setProvince(province);
            showPlateNumberKeyboard();
        });
        mBinding.settingPlateNumberKeyboard.setOnKeyPressListener(new PlateNumberKeyboardView.OnKeyPressListener() {
            @Override
            public void onKeyPress(final String key) {
                final int start = mBinding.settingPlateNumberNumber.getSelectionStart();
                final String text = mBinding.settingPlateNumberNumber.getText().toString();
                setFinishButtonState(text);
                mBinding.settingPlateNumberFinish.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
                mBinding.settingPlateNumberFinish.setBackground(
                        ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));

                // 第一位只能输入字母
                if (text.isEmpty()) {
                    if (key.matches("[A-Z]")) {  // 只允许输入字母
                        mBinding.settingPlateNumberNumber.getText().insert(start, key);
                        mBinding.settingPlateNumberClear.setVisibility(View.VISIBLE);
                    } else if (key.matches("[0-9]")) {  // 只允许输入数字
                        ToastUtils.Companion.getInstance().showCustomToastView(
                                ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_first_tip));
                    }
                    return;
                }
                // 限制最大输入长度
                if (!isEVCar() && text.length() < 6) {
                    mBinding.settingPlateNumberNumber.getText().insert(start, key);
                } else if (isEVCar() && text.length() < 7) {
                    mBinding.settingPlateNumberNumber.getText().insert(start, key);
                }
            }
            @Override
            public void onDelete() {
                final int start = mBinding.settingPlateNumberNumber.getSelectionStart();
                final int end = mBinding.settingPlateNumberNumber.getSelectionEnd();
                if (start == end && start > 0) {
                    mBinding.settingPlateNumberNumber.getText().delete(start - 1, start);  // 没有选中文本，删除光标前一个字符
                } else if (start < end) {
                    // 删除选中的文本
                    mBinding.settingPlateNumberNumber.getText().delete(start, end);
                }
                final String text = mBinding.settingPlateNumberNumber.getText().toString();
                if (!isEVCar() && text.length() >= 6) {
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setAlpha(1.0f);
                } else if (isEVCar() && text.length() >= 7){
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setAlpha(1.0f);
                } else {
                    mBinding.settingPlateNumberFinish.setEnabled(false);
                    mBinding.settingPlateNumberFinish.setAlpha(0.5f);
                }
                mBinding.settingPlateNumberFinish.setTextColor(
                        ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
                mBinding.settingPlateNumberFinish.setBackground(
                        ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
                if (text.isEmpty() && !Objects.equals(mPlateNumber, "")) {
                    mIsClearPlateNumber = true;
                    mBinding.settingPlateNumberFinish.setEnabled(true);
                    mBinding.settingPlateNumberFinish.setAlpha(1.0f);
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

    /**
     * 隐藏系统键盘
     */
    private void hideSoftKeyboard() {
        if (getActivity() == null || getActivity().getWindow() == null) {
            return;
        }
        getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
    }

    /**
     * 设置完成按钮状态
     * @param text 车牌号码
     */
    private void setFinishButtonState(final String text) {
        if (!isEVCar() && text.length() >= 5) {
            mBinding.settingPlateNumberFinish.setEnabled(true);
            mBinding.settingPlateNumberFinish.setAlpha(1.0f);
        } else if (isEVCar() && text.length() >= 6){
            mBinding.settingPlateNumberFinish.setEnabled(true);
            mBinding.settingPlateNumberFinish.setAlpha(1.0f);
        } else {
            mBinding.settingPlateNumberFinish.setEnabled(false);
            mBinding.settingPlateNumberFinish.setAlpha(0.5f);
        }
        mViewModel.setPlateNumberString(text);
    }

    @Override
    public void onInitData() {

    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        restoreFragment();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
    }

    /**
     * 恢复fragment
     */
    private void restoreFragment(){
        if(mViewModel.getIsDeletePlateNumberDialog()){
            mDeletePlateNumberDialog.show();
        }
    }

    /**
     * 清除dialog
     */
    private void clearDialog(){
        if(mDeletePlateNumberDialog.isShowing()){
            mDeletePlateNumberDialog.dismiss();
        }
        mDeletePlateNumberDialog = null;
    }

    /**
     * 初始化删除车牌号码对话框
     */
    public void initDialog() {
        mDeletePlateNumberDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_clear))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_clear_tip))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.favorite_item_delete))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setIsDeletePlateNumberDialog(false);
                        mIsClearPlateNumber = false;
                        SettingUpdateObservable.getInstance().setPlateNumber("");
                        closeFragment(true);
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setIsDeletePlateNumberDialog(false);
                    }
                }).build();

        clearBackground(mDeletePlateNumberDialog.getWindow());
    }

    /**
     * 清除背景色
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    /**
     * 显示省份键盘
     */
    public void showProvinceKeyboard(){
        mBinding.settingPlateNumberNumber.clearFocus();
        mBinding.settingPlateNumberKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.VISIBLE);
        mBinding.settingProvinceKeyboard.setVisibility(View.VISIBLE);
    }

    /**
     * 显示车牌号码键盘
     */
    public void showPlateNumberKeyboard() {
        mBinding.settingPlateNumberNumber.setFocusable(true);
        mBinding.settingPlateNumberNumber.requestFocus();
        mBinding.settingPlateNumberNumber.setSelection(mBinding.settingPlateNumberNumber.length());
        mViewModel.setIsFocus(true);
        mBinding.settingProvinceKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.VISIBLE);
        mBinding.settingPlateNumberKeyboard.setVisibility(View.VISIBLE);
    }

    /**
     * 隐藏键盘
     */
    public void hideKeyboard(){
        mBinding.settingPlateNumberKeyboardLayout.setVisibility(View.GONE);
        mBinding.settingProvinceKeyboard.setVisibility(View.GONE);
        mBinding.settingPlateNumberKeyboard.setVisibility(View.GONE);
    }

    /**
     * 校验车牌号
     * @param carNumber 车牌号
     * @return 是否为车牌号
     */
    private boolean isCar(final String carNumber) {
        if (TextUtils.isEmpty(carNumber)) {
            return false;
        } else if (isEVCar()){
            return carNumber.matches(EVCARMATCH);
        } else {
            return carNumber.matches(CARMATCH);
        }
    }

    /**
     * 处理完成按钮点击结果
     */
    public void plateNumberInputFinish() {

        Logger.d("plateNumberInputFinish");

        final String plateNumber = mBinding.settingPlateNumberProvince.getText().toString() + mBinding.settingPlateNumberNumber.getText().toString();
        if (isCar(plateNumber)) {
            SettingUpdateObservable.getInstance().setPlateNumber(plateNumber);
            closeFragment(true);
        } else if (mIsClearPlateNumber){
            mViewModel.setIsDeletePlateNumberDialog(true);
            mDeletePlateNumberDialog.show();
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_check_not_available));
        }
    }

    /**
     * 设置车牌号
     */
    private void setPlateNumber(){
        final String plateNumber = mViewModel.getPlateNumber();
        this.mPlateNumber = plateNumber;
        if (plateNumber.isEmpty()) {
            mBinding.settingProvinceKeyboard.setSelectedProvince(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_guide_plate_number_default));
            mBinding.settingPlateNumberClear.setVisibility(View.GONE);
            return;
        }
        mBinding.settingPlateNumberClear.setVisibility(View.VISIBLE);
        mBinding.settingPlateNumberFinish.setEnabled(true);
        mBinding.settingPlateNumberFinish.setAlpha(1.0f);
        mBinding.settingPlateNumberFinish.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
        mBinding.settingPlateNumberFinish.setBackground(
                ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_setting_preference_select));
        mBinding.settingPlateNumberProvince.setText(plateNumber.substring(0, 1));
        mBinding.settingProvinceKeyboard.setSelectedProvince(plateNumber.substring(0, 1));
        mBinding.settingPlateNumberNumber.setText(plateNumber.substring(1));
    }

    /**
     * 清除车牌号
     */
    public void clearPlateNumber(){
        if (!Objects.equals(mPlateNumber, "")) {
            mIsClearPlateNumber = true;
        }
        mBinding.settingPlateNumberNumber.setText("");
        mBinding.settingPlateNumberFinish.setEnabled(false);
        mBinding.settingPlateNumberFinish.setAlpha(0.5f);
    }

    /**
     * 判断是否为新能源汽车
     * @return 是否为新能源汽车
     */
    public boolean isEVCar() {
        final int carMode = CalibrationPackage.getInstance().powerType();
        return carMode == 1;
    }
}
