package com.fy.navi.service.adapter.user.usertrack;

import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;


public interface IUserTrackApi {

    /**
     * 初始化服务
     */
    void initUserTrackService();

    /**
     * 注册回调
     * @param key 回调key
     * @param callBack 回调
     */
    void registerCallBack(String key, UserTrackAdapterCallBack callBack);

    /**
     * 移除回调
     * @param key 回调key
     */
    void unRegisterCallback(String key);

    /**
     * 反初始化服务
     */
    void unInitUserTrackService();

    /**
     * 判断服务是否初始化
     * @return 初始化状态
     */
    int isInit();

    /**
     * 启动Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @param un32MsecRate 打点频率（单位毫秒，值不低于500），如传入5000表示，每5000毫秒打一个GPS点
     * @return 结果值
     */
    int startGpsTrack(String psSavePath, String psFileName, long un32MsecRate);

    /**
     * 关闭Gps打点，并生成轨迹文件
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    int closeGpsTrack(String psSavePath, String psFileName);

    /**
     * 获取指定轨迹文件的深度信息，通过异步回调返回。
     * @param psSavePath GPS轨迹文件保存路径
     * @param psFileName GPS轨迹文件名
     * @return 结果值
     */
    int obtainGpsTrackDepInfo(String psSavePath, String psFileName);

    /**
     * 获取搜索历史记录列表
     * @return 搜索历史记录列表
     */
    ArrayList<SearchHistoryItemBean> getSearchHistory();

    /**
     * 添加搜索历史记录
     * @param item 搜索历史记录
     * @return 添加结果
     */
    int addSearchHistory(SearchHistoryItemBean item);

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @param name 搜索历史记录名称
     * @return 删除结果
     */
    int delSearchHistory(String name);

    /**
     * 删除所有搜索历史记录
     * @param mode 同步方式
     * @return 删除结果
     */
    int clearSearchHistory(int mode);

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param id 行程ID
     * @param data 行程信息
     * @return 设置结果
     */
    int setBehaviorData(String id, String data);

    /**
     * 获取行为数据
     * @param type 行为数据类型
     * @param id 行为数据ID
     * @return 行为数据
     */
    String getBehaviorData(int type, String id);

    /**
     * 清空行为数据
     * @param type 行为数据类型
     * @param mode 同步方式
     * @return 清空结果
     */
    int clearBehaviorData(int type, int mode);

    /**
     * 根据ID删除行程信息
     * @param id 行程ID
     * @return 删除结果
     */
    int delBehaviorData(String id);

    /**
     * 从sdk获取行程数据列表
     */
    void getDrivingRecordData();

    /**
     * 获取行程数据列表（默认导航历史）
     * @return 行程数据列表
     */
    ArrayList<DrivingRecordDataBean> getDrivingRecordDataList();

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return 行程数据列表
     */
    ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList();

    /**
     * 获取所有行程总里程
     * @return 所有行程总里程
     */
    int getTotalDuration();

    /**
     * 获取所有行程总时长
     * @return 所有行程总时长
     */
    int getTotalDistance();

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id 行程ID
     * @return 文件路径
     */
    String getFilePath(String id);

}
