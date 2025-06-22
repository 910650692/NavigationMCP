package com.sgm.navi.hmi.setting.guide;

import android.os.Bundle;
import android.view.View;
import android.widget.CompoundButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingNaviBinding;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.Objects;


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
    public void onDestroy() {
        super.onDestroy();
        mViewModel.onDestroy();
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
    public void onViewCreated(@NonNull final View view, @Nullable final Bundle savedInstanceState) {

    }

    /**
     * 更新车牌号码
     * @param plateNumber 车牌号码
     */
    public void onPlateNumberChanged(final String plateNumber) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.naviCarNumber.setText(plateNumber);
        });
    }

    /**
     * 设置车牌号码
     * @param plateNumber 车牌号码
     */
    public void setPlateNumber(final String plateNumber) {
        ThreadManager.getInstance().postUi(() -> {
            if (!Objects.equals(plateNumber, "")) {
                mBinding.naviCarNumber.setText(plateNumber);
            }
        });
    }

    /**
     * 设置界面CheckBox点击监听
     */
    public void updateCheckBoxTextColor() {
        mBinding.fontSizeStandard.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.fontSizeLarge.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviHeadUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviNorthUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.navi3DHeadUp.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
    }

    /**
     * 更新CheckBox文字颜色
     * @param compoundButton CheckBox
     * @param isSelected 选中状态
     */
    public void updateCheckBoxTextColor(final CompoundButton compoundButton, final boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
        } else {
            compoundButton.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
        }
    }

    /**
     * 设置 AvoidLimit 状态
     * @param isTrue true 开启 false 关闭
     */
    public void setAvoidStatus(final boolean isTrue) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.naviAvoidLimit.setEnabled(isTrue);
            mBinding.naviAvoidLimitOffline.setVisibility(isTrue? View.GONE : View.VISIBLE);
            mBinding.naviAvoidLimitOffline.setEnabled(!isTrue);
        });
    }
}
