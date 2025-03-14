package com.fy.navi.hmi.setting.guide;

import android.os.Bundle;
import android.view.View;
import android.widget.CompoundButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingNaviBinding;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Objects;

/**
 * @Description 设置 - 出行页
 * @Author zxh
 * @date 2025/01/10
 */
public class SettingNaviFragment extends BaseFragment<FragmentSettingNaviBinding, SettingGuideViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_navi;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        //获取初始化数据
        mViewModel.initView();
        updateCheckBoxTextColor();
    }

    @Override
    public void onInitData() {

    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {

    }

    public void onPlateNumberChanged(String plateNumber) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.naviCarNumber.setText(plateNumber);
        });
    }

    public void setPlateNumber(String plateNumber) {
        ThreadManager.getInstance().postUi(() -> {
            if (!Objects.equals(plateNumber, "")) {
                mBinding.naviCarNumber.setText(plateNumber);
            }
        });
    }

    public void updateCheckBoxTextColor() {
        mBinding.chargingStation.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.favoritePoint.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.roadCondition.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.fontSizeStandard.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.fontSizeLarge.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviHeadUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviNorthUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.navi3DHeadUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.settingGuideCarLogoDefault.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.settingGuideCarLogoBrand.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.settingGuideCarLogoSpeed.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
    }

    public void updateCheckBoxTextColor(CompoundButton compoundButton, boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(com.fy.navi.scene.R.color.white));
        } else {
            compoundButton.setTextColor(getResources().getColor(com.fy.navi.scene.R.color.setting_preference_text_gray));
        }
    }

}
