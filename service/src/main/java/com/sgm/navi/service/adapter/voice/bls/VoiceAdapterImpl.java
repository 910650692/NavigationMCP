package com.sgm.navi.service.adapter.voice.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.data.VoiceService;
import com.autonavi.gbl.data.model.Voice;
import com.autonavi.gbl.data.model.VoiceInitConfig;
import com.autonavi.gbl.data.observer.IDataInitObserver;
import com.autonavi.gbl.data.observer.IDataListObserver;
import com.autonavi.gbl.data.observer.IDownloadObserver;
import com.autonavi.gbl.data.observer.IImageObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.adapter.voice.VoiceAdapterCallback;
import com.sgm.navi.service.adapter.voice.VoiceApi;
import com.sgm.navi.service.define.voice.DownLoadMode;
import com.sgm.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class VoiceAdapterImpl implements VoiceApi, IDataInitObserver, IDataListObserver, IImageObserver, IDownloadObserver {
    private static final String TAG = VoiceAdapterImpl.class.getSimpleName();
    private final Hashtable<String, VoiceAdapterCallback> callbacks;
    private VoiceService voiceService;
    ConcurrentHashMap<Integer, VoiceInfo> recommendVoiceList;

    public VoiceAdapterImpl() {
        callbacks = new Hashtable<>();
        recommendVoiceList = new ConcurrentHashMap<>();

    }
    @Override
    public void initService() {
        Logger.d(TAG, "initService");
        if(voiceService == null){
            Logger.e("voiceService is null");
            voiceService = (VoiceService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.VoiceDataSingleServiceID);
        }
        VoiceInitConfig config = new VoiceInitConfig();
        config.flytekStoredPath = GBLCacheFilePath.VOICE_FLY_PATH;
        config.mitStoredPath = GBLCacheFilePath.VOICE_MIT_PATH;
        // voicedata.json 配置文件所存放的目录
        config.configfilePath = GBLCacheFilePath.VOICE_CONF_PATH;
        // 设置磁盘空间安全阈值（默认设置为80MB）
        config.thresholdValue = 80;
        FileUtils.getInstance().createDir(config.configfilePath);
        FileUtils.getInstance().createDir(config.flytekStoredPath);

        voiceService.init(config, this);
        voiceService.addNetDownloadObserver(this);
        voiceService.addUsbDownloadObserver(this);
    }

    /**
     * 服务反初始化方法
     * 当不再使用服务时调用此方法进行清理和资源释放
     * 具体操作是检查voiceService对象是否非空，若非空，则调用其unInit方法进行反初始化
     * 这样做是为了确保合理地管理资源，避免内存泄漏
     */
    @Override
    public void unInitService() {
        if(voiceService != null){
            voiceService.removeNetDownloadObserver(this);
            voiceService.removeUsbDownloadObserver(this);
            voiceService.unInit();
        }
    }

    /**
     * 检查语音服务是否已经初始化
     *
     * 当需要确定语音服务是否已经准备好并可以使用时，调用此方法
     * 如果语音服务实例（voiceService）不为空，则进一步调用该实例的isInit方法来判断初始化状态
     * 否则，返回0，表示服务未初始化或服务实例不存在
     *
     * @return int 服务的初始化状态如果服务已初始化，则返回非零值；如果未初始化或服务实例为空，则返回0
     */
    @Override
    public int isInitService() {
        if(voiceService != null){
            return voiceService.isInit();
        }
        return 0;
    }

    @Override
    public void registerCallback(String key, VoiceAdapterCallback resultCallback) {
        callbacks.put(key, resultCallback);
    }

    /**
     * 检测数据列表
     * @param downLoadMode 数据下载模式
     * @param path 路径
     * @return 返回错误码
     */
    @Override
    public int requestDataListCheck(int downLoadMode, String path) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return 0;
        }
        return voiceService.requestDataListCheck(downLoadMode, path, this);
    }

    /**
     * 终止行政区域表单网络请求
     * @param downLoadMode 下载模式
     */
    @Override
    public int abortRequestDataListCheck(int downLoadMode) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return 0;
        }
        return voiceService.abortRequestDataListCheck(downLoadMode);

    }

    /**
     * 获取所有语音包id列表
     * @param downLoadMode 下载模式
     * @return 语音包id列表
     */
    @Override
    public ArrayList<Integer> getVoiceIdList(int downLoadMode) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return new ArrayList<>();
        }
        return voiceService.getVoiceIdList(downLoadMode);
    }

    /**
     * 获取指定类型(讯飞/MIT)的语音包id列表
     * @param downloadMode 下载模式
     * @param engineType 语音引擎类型
     * @return 语音包id列表
     */
    @Override
    public ArrayList<Integer> getVoiceIdList(int downloadMode, int engineType) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return new ArrayList<>();
        }
        return voiceService.getVoiceIdList(downloadMode, engineType);
    }

    @Override
    public Map<Integer, VoiceInfo> getRecommendVoiceList() {
        if(recommendVoiceList == null || recommendVoiceList.isEmpty()){
            ConcurrentHashMap<Integer, VoiceInfo> voiceList = new ConcurrentHashMap<>();
            ArrayList<Integer> voiceIdList = getVoiceIdList(DownLoadMode.DOWNLOAD_MODE_NET.ordinal());
            if (voiceIdList != null && voiceIdList.size() > 0) {
                for (int i = 0; i < voiceIdList.size(); i++) {
                    VoiceInfo voiceInfo = getVoice(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), voiceIdList.get(i));
                    if (voiceInfo != null) {
                        Logger.d(TAG, "voiceInfo: ", voiceInfo.getName(),
                                voiceInfo.getId(), voiceInfo.getFilePath());
                        voiceList.put(voiceInfo.getId(), voiceInfo);
                    }
                }
            }
            recommendVoiceList = voiceList;
        }
        return recommendVoiceList;
    }

    /**
     * 根据语音包id获取语音信息
     * @param voiceId 语音包id
     * @return 语音信息
     */
    @Override
    public VoiceInfo getVoice(int downloadMode, int voiceId) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return new VoiceInfo();
        }
        Voice voice =  voiceService.getVoice(downloadMode,voiceId);
        if(voice != null){
            return GsonUtils.convertToT(voice, VoiceInfo.class);
        }else{
            Logger.e("voice is null");
            return new VoiceInfo();
        }
    }

    /**
     * 网络请求语音头像
     * @param downloadMode 数据下载模式，当前仅支持 DOWNLOAD_MODE_NET
     * @param voiceId 语音记录Id
     * @return 返回错误码
     */
    @Override
    public int requestDataImage(int downloadMode, int voiceId) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return 0;
        }
        return voiceService.requestDataImage(downloadMode, voiceId, this);

    }

    /**
     * 终止网络请求语音头像
     * @param downloadMode 数据下载模式
     * @param voiceId 	语音记录Id
     * @return 返回错误码
     */
    @Override
    public int abortRequestDataImage(int downloadMode, int voiceId) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return 0;
        }
        return voiceService.abortRequestDataImage(downloadMode, voiceId);

    }

    /**
     * 下载操作
     * @param opType    操作类型
     * @param voiceIdDiyLst 语音ID列表
     * @return 返回错误码
     */
    @Override
    public int operate(int opType, ArrayList<Integer> voiceIdDiyLst) {
        if(voiceService == null){
            Logger.e("voiceService is null");
            return 0;
        }
        return voiceService.operate(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), opType, voiceIdDiyLst);

    }

    /**
     * @param downLoadMode 下载模式
     * @param dataType 数据类型
     * @param opCode 错误码
     */
    @Override
    public void onInit(int downLoadMode, int dataType, int opCode) {
        Logger.d(TAG, "VoiceService onInit: downLoadMode=" + downLoadMode
                + "dataType=" + dataType + "opCode=" + opCode);

        if(Service.ErrorCodeOK == opCode){
            Logger.d(TAG, "VoiceService 初始化成功");
            requestDataListCheck(DownLoadMode.DOWNLOAD_MODE_NET.ordinal(), "");
        } else {
            Logger.d(TAG, "VoiceService 初始化失败");
        }

        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onInit(downLoadMode, dataType, opCode);
        }
    }

    /**
     * 拉取头像观察者回调
     * @param itemId 数据编号
     * @param opErrCode 回调操作状态码
     * @param strFilePath 文件下载存放的绝对路径
     * @param dataType 	数据类型
     */
    @Override
    public void onDownloadImage(int itemId, int opErrCode, String strFilePath, int dataType) {
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onDownloadImage(itemId, opErrCode, strFilePath, dataType);
        }
    }

    /**
     * 数据列表获校验请求回调
     * @param downLoadMode 下载模式
     * @param dataType 数据类型
     * @param opCode 错误码
     */
    @Override
    public void onRequestDataListCheck(int downLoadMode, int dataType, int opCode) {
        Logger.d(TAG, "VoiceService onRequestDataListCheck: downLoadMode=" + downLoadMode
                + "dataType=" + dataType + "opCode=" + opCode);
        getRecommendVoiceList();
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onRequestDataListCheck(downLoadMode, dataType, opCode);
        }
    }

    /**
     * 下载状态回调
     * @param downLoadMode 下载模式
     * @param dataType 数据类型
     * @param id 数据id
     * @param taskCode 任务状态
     * @param opCode 操作状态码
     */
    @Override
    public void onDownLoadStatus(int downLoadMode, int dataType, int id, int taskCode, int opCode) {
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onDownLoadStatus(downLoadMode, dataType, id, taskCode, opCode);
        }
    }

    /**
     * 下载进度回调
     * @param downLoadMode 下载模式
     * @param dataType 	数据类型
     * @param id 数据id
     * @param percentType 百分比类型 (默认0表示下载; 1表示解压融合进度)
     * @param percent 百分比值
     */
    @Override
    public void onPercent(int downLoadMode, int dataType, int id, int percentType, float percent) {
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onPercent(downLoadMode, dataType, id, percentType, percent);
        }
    }

    /**
     * 当下载任务被操作时调用的方法
     *
     * @param downLoadMode 下载模式，表示下载任务的类型
     * @param dataType 数据类型，表示被操作数据的类别
     * @param opType 操作类型，表示对下载任务执行的具体操作
     * @param opreatedIdList 被操作的ID列表，包含受到影响的下载任务ID
     */
    @Override
    public void onOperated(int downLoadMode, int dataType, int opType, ArrayList<Integer> opreatedIdList) {
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (VoiceAdapterCallback callBack : callbacks.values()) {
            if (callBack == null) continue;
            callBack.onOperated(downLoadMode, dataType, opType, opreatedIdList);
        }
    }
}
