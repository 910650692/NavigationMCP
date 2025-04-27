package com.fy.navi.service.logicpaket.voice;

import java.util.ArrayList;

public interface VoiceCallback {

    /**
     * @param downLoadMode 下载模式
     * @param dataType     数据类型
     * @param opCode       错误码
     */
    default void onInit(int downLoadMode, int dataType, int opCode) {

    }

    /**
     * 拉取头像观察者回调
     *
     * @param itemId      数据编号
     * @param opErrCode   回调操作状态码
     * @param strFilePath 文件下载存放的绝对路径
     * @param dataType    数据类型
     */
    default void onDownloadImage(int itemId, int opErrCode, String strFilePath, int dataType) {

    }

    /**
     * 数据列表获校验请求回调
     *
     * @param downLoadMode 下载模式
     * @param dataType     数据类型
     * @param opCode       错误码
     */
    default void onRequestDataListCheck(int downLoadMode, int dataType, int opCode) {

    }

    /**
     * 下载状态回调
     * @param downLoadMode 下载模式
     * @param dataType 数据类型
     * @param id 数据id
     * @param taskCode 任务状态
     * @param opCode 操作状态码
     */
    void onDownLoadStatus(int downLoadMode, int dataType, int id, int taskCode, int opCode);

    /**
     * 下载进度回调
     * @param downLoadMode 下载模式
     * @param dataType 数据类型
     * @param id 数据id
     * @param percentType 百分比类型 (默认0表示下载; 1表示解压融合进度)
     * @param percent 百分比值
     */
    void onPercent(int downLoadMode, int dataType, int id, int percentType, float percent);

    /**
     * 当下载任务被操作时调用的方法
     *
     * @param downLoadMode 下载模式，表示下载任务的类型
     * @param dataType 数据类型，表示被操作数据的类别
     * @param opType 操作类型，表示对下载任务执行的具体操作
     * @param opreatedIdList 被操作的ID列表，包含受到影响的下载任务ID
     */
    void onOperated(int downLoadMode, int dataType, int opType, ArrayList<Integer> opreatedIdList);
}
