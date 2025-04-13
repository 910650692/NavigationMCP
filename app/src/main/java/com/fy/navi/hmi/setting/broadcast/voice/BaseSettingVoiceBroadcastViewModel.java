package com.fy.navi.hmi.setting.broadcast.voice;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.R;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.voice.OperationStatus;
import com.fy.navi.service.define.voice.VoiceInfo;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;
import java.util.HashMap;

public class BaseSettingVoiceBroadcastViewModel extends BaseViewModel<SettingVoiceBroadcastFragment, SettingVoiceBroadcastModel> {

    private static final String TAG = BaseSettingVoiceBroadcastViewModel.class.getSimpleName();

    HashMap<Integer, VoiceInfo> recommendVoiceList = new HashMap<>();
    public MutableLiveData<Boolean> mIsDefaultVoiceUsed = new MutableLiveData<>(true);

    public BaseSettingVoiceBroadcastViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected SettingVoiceBroadcastModel initModel() {
        return new SettingVoiceBroadcastModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action mSwitchVoiceBroadcastClick = () -> {
        closeFragment(true);
    };

    public Action mSwitchDefaultVoice = () -> {
        if(Boolean.FALSE.equals(mIsDefaultVoiceUsed.getValue())){
            mIsDefaultVoiceUsed.setValue(true);
            mModel.setVoice(GBLCacheFilePath.DEFAULT_VOICE_PATH);
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_PACKAGE, "default");
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_ICON, "default");
            SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_NAME,
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_broadcast_voice_current_name));
            mView.setCurrentVoice();
            mView.unSelectAllVoices();

            sendBuryPointForSetVoice(ResourceUtils.Companion.getInstance().getString(R.string.setting_broadcast_voice_current_name));
        }
    };
    /**
     * 数据列表检查
     * @param downLoadMode
     * @param dataType
     * @param opCode
     */
    public void onRequestDataListCheck(final int downLoadMode, final int dataType, final int opCode){
        if(opCode == 0){
            recommendVoiceList = mModel.getRecommendVoiceList();
            mView.setData(recommendVoiceList);
        }
    }

    /**
     * 下载状态回调
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param taskCode
     * @param opCode
     */
    public void onDownLoadStatus(final int downLoadMode, final int dataType, final int id, final int taskCode, final int opCode) {
        ThreadManager.getInstance().postUi(()->{
            if(opCode == UserDataCode.OPT_NET_DISCONNECT){
                ToastUtils.Companion.getInstance().showCustomToastView("无网络连接，请检查网络后重试");
            } else if (opCode == UserDataCode.OPT_DOWNLOAD_NET_ERROR) {
                ToastUtils.Companion.getInstance().showCustomToastView("网络异常，请检查网络后重试");
            }
                }
        );
        replaceVoiceInfo(id, taskCode, 0);
    }

    /**
     * 下载进度回调
     * @param downLoadMode
     * @param dataType
     * @param id
     * @param percentType
     * @param percent
     */
    public void onPercent(final int downLoadMode, final int dataType, final int id, final int percentType, final float percent){
        final int taskCode = (percentType == 0) ? OperationStatus.TASK_STATUS_CODE_DOING : OperationStatus.TASK_STATUS_CODE_UNZIPPING;
        replaceVoiceInfo(id, taskCode, percent);
    }

    public int isInitService(){
        return mModel.isInitService();
    }

    /**
     * 检查数据列表
     * @param downLoadMode
     * @param path
     */
    public void requestDataListCheck(final int downLoadMode, final String path){
        mModel.requestDataListCheck(downLoadMode, path);
    }

    /**
     * 操作
     * @param operatedIdList
     */
    public void startAllTask(final ArrayList<Integer> operatedIdList){
        mModel.operate(UserDataCode.OPERATION_TYPE_START, operatedIdList);
    }

    /**
     * 操作
     * @param operatedIdList
     */
    public void pauseAllTask(final ArrayList<Integer> operatedIdList){
        mModel.operate(UserDataCode.OPERATION_TYPE_PAUSE, operatedIdList);
    }

    /**
     * 操作
     * @param voiceInfo
     */
    public void toUseAllTask(final VoiceInfo voiceInfo){
        ThreadManager.getInstance().postDelay(() -> {
            mModel.setVoice(voiceInfo.getFilePath());
        },0);
        mIsDefaultVoiceUsed.setValue(false);
        SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_PACKAGE, String.valueOf(voiceInfo.getId()));
        SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_ICON, voiceInfo.getImageUrl());
        SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_NAME, voiceInfo.getName());
        mView.setCurrentVoice();
        mView.setSingleChoice(voiceInfo.getId());

        sendBuryPointForSetVoice(voiceInfo.getName());
    }

    /**
     * 更新语音信息
     * @param id
     * @param taskCode
     * @param percent
     */
    public void replaceVoiceInfo(final int id, final int taskCode, final float percent){
        final VoiceInfo voiceInfo = recommendVoiceList.get(id);
        if(voiceInfo != null){
            voiceInfo.setTaskState(taskCode);
            voiceInfo.setPercent(percent);
            recommendVoiceList.replace(id, voiceInfo);
            mView.updateItem(id, voiceInfo);
        }else{
            Logger.e(TAG, "No id: " + id);
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_SETTING_VOICEPACKAGE)
    private void sendBuryPointForSetVoice(String name){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SETTING_CONTENT, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }
}
