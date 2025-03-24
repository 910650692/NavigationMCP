package com.fy.navi.hmi.setting.broadcast.voice;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentVoiceBroadcastBinding;
import com.fy.navi.hmi.setting.broadcast.adapter.SettingVoiceBroadcastAdapter;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.voice.DownLoadMode;
import com.fy.navi.service.define.voice.VoiceInfo;
import com.fy.navi.service.define.voice.VoiceServiceInitStatus;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.HashMap;
import java.util.Objects;

public class SettingVoiceBroadcastFragment extends BaseFragment<FragmentVoiceBroadcastBinding,
        SettingVoiceBroadcastViewModel> implements SettingVoiceBroadcastAdapter.OnItemClickListener{

    private static final String TAG = SettingVoiceBroadcastFragment.class.getSimpleName();

    private SettingVoiceBroadcastAdapter mSettingVoiceBroadcastAdapter;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_voice_broadcast;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        initView();
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

    @Override
    public void onInitData() {
        initData();
    }

    /**
     * 初始化
     */
    private void initView() {
        mSettingVoiceBroadcastAdapter = new SettingVoiceBroadcastAdapter();
        mSettingVoiceBroadcastAdapter.setItemClickListener(this);
        final LinearLayoutManager manager = new LinearLayoutManager(getActivity());
        manager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.recommendVoiceList.setNestedScrollingEnabled(true);
        mBinding.recommendVoiceList.setLayoutManager(manager);
        mBinding.recommendVoiceList.setAdapter(mSettingVoiceBroadcastAdapter);
    }

    /**
     * 初始化数据
     */
    private void initData() {
        setCurrentVoice();
        if(mViewModel.isInitService() == VoiceServiceInitStatus.ServiceInitDone.ordinal()) {
            mViewModel.requestDataListCheck(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), "");
        }
    }
    /**
     * 设置数据
     * @param voiceInfoList
     */
    public void setData(final HashMap<Integer, VoiceInfo> voiceInfoList) {
        final String selectedVoice = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_PACKAGE);
        if(selectedVoice != null && !selectedVoice.isEmpty()){
            if(Objects.equals(selectedVoice, "default")){
                mViewModel.mIsDefaultVoiceUsed.setValue(true);
            } else {
                mViewModel.mIsDefaultVoiceUsed.setValue(false);
                final int voiceId = Integer.parseInt(selectedVoice);
                for(VoiceInfo voiceInfo : voiceInfoList.values()){
                    if(Objects.equals(voiceInfo.getId(), voiceId)){
                        voiceInfo.setUsed(true);
                        voiceInfoList.replace(voiceInfo.getId(), voiceInfo);
                        break;
                    }
                }
            }
        }
        mSettingVoiceBroadcastAdapter.setData(voiceInfoList);
    }

    /**
     * 更新数据
     * @param voiceInfoList
     */
    public void updateData(final HashMap<Integer, VoiceInfo> voiceInfoList) {
        mSettingVoiceBroadcastAdapter.setData(voiceInfoList);
    }

    /**
     * 单选设置
     * @param index
     */
    public void setSingleChoice(final int index) {
        mSettingVoiceBroadcastAdapter.setSingleChoice(index);
    }

    /**
     * 设置数据列表为Fale
     */
    public void unSelectAllVoices() {
        mSettingVoiceBroadcastAdapter.unSelectAllVoices();
    }

    @Override
    public void onOperation(final int index) {
        Logger.d("SettingVoiceBroadcastModel", "onOperationStart index: " + index);
        mViewModel.toOperate(index);
    }

    /**
     * 设置当前声音
     */
    public void setCurrentVoice() {
        final String selectedVoice = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_PACKAGE);
        final String name = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_NAME);
        final String icon = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_VOICE_ICON);
        if(selectedVoice != null && !selectedVoice.isEmpty()){
            if(Objects.equals(selectedVoice, "default")){
                mBinding.currentVoiceBroadcastCurrentName.setText(R.string.setting_broadcast_voice_current_name);
                mBinding.currentVoiceBroadcastHeader.setImageResource(R.mipmap.default_voice);
            } else {
                if(name != null && !name.isEmpty()){
                    mBinding.currentVoiceBroadcastCurrentName.setText(name);
                }
                if (icon != null && !icon.isEmpty()){
                    Logger.d("SettingVoiceBroadcastModel", "setCurrentVoice icon: " + icon);
                    ViewAdapterKt.loadImageUrl(mBinding.currentVoiceBroadcastHeader, icon, R.mipmap.default_user_icon, R.mipmap.default_user_icon);
                }
            }
        }

    }
}
