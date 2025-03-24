package com.fy.navi.hmi.setting.broadcast.voice;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.voice.DownLoadMode;
import com.fy.navi.service.define.voice.OperationStatus;
import com.fy.navi.service.define.voice.OperationType;
import com.fy.navi.service.define.voice.VoiceInfo;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

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
     * @param index
     */
    public void toOperate(final int index){
        Logger.d("SettingVoiceBroadcastModel", "switchTaskStatusToOperate: " + index);
        final ArrayList<VoiceInfo> voiceInfoList = new ArrayList<>(recommendVoiceList.values());
        final VoiceInfo voiceInfo = voiceInfoList.get(index);
        final ArrayList<Integer> operatedIdList = new ArrayList<>();
        operatedIdList.add(voiceInfo.getId());

        Logger.d("SettingVoiceBroadcastModel", "switchTaskStatusToOperate: " + operatedIdList);

        switch (voiceInfo.getTaskState()) {
            case OperationStatus.TASK_STATUS_CODE_READY:
            case OperationStatus.TASK_STATUS_CODE_PAUSE:
            case OperationStatus.TASK_STATUS_CODE_ERR:
            case OperationStatus.TASK_STATUS_CODE_MAX:
                mModel.operate(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), OperationType.OPERATION_TYPE_START.ordinal(), operatedIdList);
                break;
            case OperationStatus.TASK_STATUS_CODE_CHECKED:
                ToastUtils.Companion.getInstance().showCustomToastView("验证完成");
                break;
            case OperationStatus.TASK_STATUS_CODE_UNZIPPED:
                ToastUtils.Companion.getInstance().showCustomToastView("解压完成");
                break;
            case OperationStatus.TASK_STATUS_CODE_SUCCESS:
                mIsDefaultVoiceUsed.setValue(false);
                mModel.setVoice(Objects.requireNonNull(voiceInfoList.get(index)).getFilePath());
                SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_PACKAGE,
                        String.valueOf(Objects.requireNonNull(voiceInfoList.get(index)).getId()));
                SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_ICON, voiceInfoList.get(index).getImageUrl());
                SettingManager.getInstance().insertOrReplace(SettingController.KEY_SETTING_VOICE_NAME, voiceInfoList.get(index).getName());
                mView.setCurrentVoice();
                mView.setSingleChoice(index);
                break;
            case OperationStatus.TASK_STATUS_CODE_DOING:
            case OperationStatus.TASK_STATUS_CODE_DONE:
            case OperationStatus.TASK_STATUS_CODE_CHECKING:
            case OperationStatus.TASK_STATUS_CODE_UNZIPPING:
            case OperationStatus.TASK_STATUS_CODE_WAITING:
                mModel.operate(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), OperationType.OPERATION_TYPE_PAUSE.ordinal(), operatedIdList);
                break;
            default:
                break;
        }
    }

    /**
     * 设置语音信息
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
            mView.updateData(recommendVoiceList);
        }else{
            Logger.e(TAG, "No id: " + id);
        }
    }
}
