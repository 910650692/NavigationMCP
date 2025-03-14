package com.fy.navi.hmi.setting.broadcast;

import android.widget.CompoundButton;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingBroadcastBinding;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Objects;

public class SettingBroadcastFragment extends BaseFragment<FragmentSettingBroadcastBinding, SettingBroadcastViewModel>{
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_broadcast;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.initView();
        setCurrentVoice();
        updateCheckBoxTextColor();
    }

    @Override
    public void onInitData() {
        // 初始化数据
    }

    public void updateCruiseBroadcastEnable(boolean isDisable) {
        mBinding.cruiseBroadcastRoadCondition.setEnabled(isDisable);
        mBinding.cruiseBroadcastCamera.setEnabled(isDisable);
        mBinding.cruiseBroadcastSafe.setEnabled(isDisable);
    }

    public void updateCheckBoxTextColor() {
        mBinding.naviBroadcastDetailed.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviBroadcastSimple.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.naviBroadcastConcise.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.cruiseBroadcastRoadCondition.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.cruiseBroadcastCamera.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
        mBinding.cruiseBroadcastSafe.setOnCheckedChangeListener(this::updateCheckBoxTextColor);
    }

    public void updateCheckBoxTextColor(CompoundButton compoundButton, boolean isSelected) {
        if (isSelected) {
            compoundButton.setTextColor(getResources().getColor(com.fy.navi.scene.R.color.white));
        } else {
            compoundButton.setTextColor(getResources().getColor(com.fy.navi.scene.R.color.setting_preference_text_gray));
        }
    }

    public void setCurrentVoice() {
        String selectedVoice = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_PACKAGE);
        String name = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_NAME);
        String icon = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_ICON);
        if(selectedVoice != null && !selectedVoice.isEmpty()){
            if(Objects.equals(selectedVoice, "default")){
                mBinding.naviBroadcastVoiceName.setText(R.string.setting_broadcast_voice_current_name);
                mBinding.naviBroadcastVoiceCurrent.setImageResource(R.mipmap.default_voice);
            } else {
                if(name != null && !name.isEmpty()){
                    mBinding.naviBroadcastVoiceName.setText(name);
                }
                if (icon != null && !icon.isEmpty()){
                    Logger.d("SettingVoiceBroadcastModel", "setCurrentVoice icon: " + icon);
                    ViewAdapterKt.loadImageUrl(mBinding.naviBroadcastVoiceCurrent, icon, R.mipmap.default_voice, R.mipmap.default_voice);
                }
            }
        }

    }
}
