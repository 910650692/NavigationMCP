package com.fy.navi.service.adapter.user.usertrack;

import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.FootprintNaviRecordInfo;
import com.fy.navi.service.define.user.usertrack.HistoryPoiItemBean;
import com.fy.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/26
 */
public interface IUserTrackApi {

    void initUserTrackService();

    void registerCallBack(String key, UserTrackAdapterCallBack callBack);

    void unRegisterCallback(String key);

    void unInitUserTrackService();

    int isInit();

    /**
     * 开始打点，生成轨迹文件
     * @param psSavePath
     * @param psFileName
     * @param un32MsecRate
     * @return
     */
    int startGpsTrack(String psSavePath, String psFileName, long un32MsecRate);

    /**
     * 停止打点，调用时机 停止导航、巡航导航切换
     * @param psSavePath
     * @param psFileName
     * @return
     */
    int closeGpsTrack(String psSavePath, String psFileName);

    /**
     * 获取轨迹文件信息
     * @param psSavePath
     * @param psFileName
     * @return
     */
    int obtainGpsTrackDepInfo(String psSavePath, String psFileName);

    /**
     * 获取搜索历史列表
     * @return
     */
    ArrayList<SearchHistoryItemBean> getSearchHistory();

    /**
     * 添加搜索历史
     * @return
     */
    int addSearchHistory(SearchHistoryItemBean item);

    /**
     * 删除搜索历史
     * @return
     */
    int delSearchHistory(String name);

    /**
     * 清除搜索历史
     * @param mode
     * @return
     */
    int clearSearchHistory(int mode);

    /**
     * 根据生成的GPS轨迹文件，构造行程数据Json串，上传到同步库
     * @param id
     * @param data
     * @return
     */
    int setBehaviorData(String id, String data);

    String getBehaviorData(int type, String id);

    int clearBehaviorData(int type, int mode);

    /**
     * 根据ID删除行程信息
     * @param id
     * @return
     */
    int delBehaviorData(String id);

    /**
     * 从sdk获取行程数据列表
     */
    void getDrivingRecordData();

    /**
     * 获取行程列表数据
     * @return
     */
    ArrayList<DrivingRecordDataBean> getDrivingRecordDataList();

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return
     */
    ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList();

    /**
     * 获取总时长（单位：秒）
     * @return
     */
    int getTotalDuration();

    /**
     * 获取总里程（单位：m）
     * @return
     */
    int getTotalDistance();

    /**
     * 获取同步库轨迹文件
     * @param id
     * @return
     */
    String getFilePath(String id);

}
