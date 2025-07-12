package com.sgm.navi.service.adapter.voice;

import com.sgm.navi.service.define.voice.VoiceInfo;

import java.util.ArrayList;
import java.util.Map;

public interface VoiceApi {

    void initService();

    void unInitService();

    int isInitService();

    void registerCallback(String key, VoiceAdapterCallback resultCallback);

    /**
     * 检测数据列表
     * @param downLoadMode 数据下载模式
     * @param path 路径
     * @return 返回错误码
     */
    int requestDataListCheck(int downLoadMode, String path);

    /**
     * 终止行政区域表单网络请求
     * @param downLoadMode 下载模式
     */
    int abortRequestDataListCheck(int downLoadMode);

    /**
     * 获取所有语音包id列表
     * @param downLoadMode 下载模式
     * @return 语音包id列表
     */
    ArrayList<Integer> getVoiceIdList(int downLoadMode);

    /**
     * 获取指定类型(讯飞/MIT)的语音包id列表
     * @param downloadMode 下载模式
     * @param engineType 语音引擎类型
     * @return 语音包id列表
     */
    ArrayList<Integer> getVoiceIdList(int downloadMode, int engineType);

    Map<Integer, VoiceInfo> getRecommendVoiceList();

    /**
     * 根据语音包id获取语音信息
     *
     * @param voiceId 语音包id
     * @return 语音信息
     */
    VoiceInfo getVoice(int	downloadMode, int voiceId);

    /**
     * 网络请求语音头像
     * @param downloadMode 数据下载模式，当前仅支持 DOWNLOAD_MODE_NET
     * @param voiceId 语音记录Id
     * @return 返回错误码
     */
    int requestDataImage(int downloadMode, int voiceId);

    /**
     * 终止网络请求语音头像
     * @param downloadMode 数据下载模式
     * @param voiceId 	语音记录Id
     * @return 返回错误码
     */
    int abortRequestDataImage(int downloadMode, int voiceId);


    /**
     * 下载操作
     * @param opType    操作类型
     * @param voiceIdDiyLst 语音ID列表
     * @return 返回错误码
     */
    int operate(int opType, ArrayList<Integer> voiceIdDiyLst);
}
