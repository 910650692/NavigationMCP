package com.sgm.navi.hmi.setting.broadcast.voice;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentVoiceBroadcastBinding;
import com.sgm.navi.hmi.setting.broadcast.adapter.SettingVoiceBroadcastAdapter;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.voice.VoiceInfo;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.ui.action.ViewAdapterKt;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

public class SettingVoiceBroadcastFragment extends BaseFragment<FragmentVoiceBroadcastBinding,
        SettingVoiceBroadcastViewModel> implements SettingVoiceBroadcastAdapter.OnItemClickListener{

    private static final String TAG = SettingVoiceBroadcastFragment.class.getSimpleName();

    private SettingVoiceBroadcastAdapter mSettingVoiceBroadcastAdapter;
    private int mVoiceId;
    private boolean mIsUsingAllTask;

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
        mSettingVoiceBroadcastAdapter = new SettingVoiceBroadcastAdapter(getActivity());
        mSettingVoiceBroadcastAdapter.setItemClickListener(this);
        final LinearLayoutManager manager = new LinearLayoutManager(getActivity());
        manager.setOrientation(LinearLayoutManager.VERTICAL);
        mBinding.recommendVoiceList.setItemAnimator(null);
        mBinding.recommendVoiceList.setLayoutManager(manager);
        mBinding.recommendVoiceList.setAdapter(mSettingVoiceBroadcastAdapter);
    }

    /**
     * 初始化数据
     */
    private void initData() {
        setCurrentVoice();
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

    public void updateItem(final int id, final VoiceInfo voiceInfo) {
        mSettingVoiceBroadcastAdapter.updateItem(id, voiceInfo);
    }

    /**
     * 单选设置
     * @param index
     */
    public void setSingleChoice(final int index) {
        Logger.d(TAG, "setSingleChoice: ", index);
        mSettingVoiceBroadcastAdapter.setSingleChoice(index);
        mIsUsingAllTask = false;
    }

    /**
     * 设置数据列表为False
     */
    public void unSelectAllVoices() {
        mSettingVoiceBroadcastAdapter.unSelectAllVoices();
    }

    @Override
    public void startAllTask(ArrayList<Integer> operatedIdList) {
        mViewModel.startAllTask(operatedIdList);
    }

    @Override
    public void pauseAllTask(ArrayList<Integer> operatedIdList) {
        mViewModel.pauseAllTask(operatedIdList);
    }

    @Override
    public void toUseVoice(final VoiceInfo voiceInfo) {
        Logger.d(TAG, "toUseVoice: ", mIsUsingAllTask, voiceInfo.getId());
        if (!mIsUsingAllTask && mVoiceId != voiceInfo.getId()) {
            mIsUsingAllTask = true;
            mVoiceId = voiceInfo.getId();
            mViewModel.toUseVoice(voiceInfo);
        }
    }

    @Override
    public void toDeleteTask(VoiceInfo voiceInfo) {
        Logger.i(TAG, "toDeleteTask: ", voiceInfo.getId());
        if (mVoiceId != voiceInfo.getId()) {
            mViewModel.toDeleteTask(voiceInfo);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView("不能删除当前使用的语音");
        }
    }

    /**
     * 获取当前使用声音
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
                    ViewAdapterKt.loadImageUrl(mBinding.currentVoiceBroadcastHeader, icon, R.drawable.img_default_user_icon, R.drawable.img_default_user_icon);
                }
            }
        }
    }

    /**
     * 设置标准女声的VoiceId
     */
    public void resetVoiceId() {
        mVoiceId = 0;
    }
}
