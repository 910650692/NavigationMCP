package com.fy.navi.service.logicpaket.user.usertrack;

import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapter;
import com.fy.navi.service.adapter.user.usertrack.UserTrackAdapterCallBack;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.GpsTrackDepthBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public class UserTrackPackage implements UserTrackAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.USER_TRACK_SERVICE_TAG;
    private final UserTrackAdapter mUserTrackAdapter;
    private final List<UserTrackCallBack> callBacks = new ArrayList<>();

    private UserTrackPackage() {
        mUserTrackAdapter = UserTrackAdapter.getInstance();
    }

    public void initUserTrackService() {
        mUserTrackAdapter.initUserTrackService();
        mUserTrackAdapter.registerCallBack("UserTrackPackage", this);
    }

    public synchronized void registerCallBack(UserTrackCallBack callback) {
        if (callback != null && !callBacks.contains(callback)) {
            callBacks.add(callback);
        }
    }

    public void unInitUserTrackService() {
        mUserTrackAdapter.unInitUserTrackService();
    }

    /**
     * 获取搜索历史记录列表
     * @return
     */
    public ArrayList<SearchHistoryItemBean> getSearchHistory() {
        return mUserTrackAdapter.getSearchHistory();
    }

    /**
     * 添加搜索历史记录
     * @param item
     * @return
     */
    public int addSearchHistory(SearchHistoryItemBean item) {
        /*// 同步模式
        int mode = UserTrackModeType.SyncModeNow; // 立即同步
        // 2.1 搜索历史 关键字
        SearchHistoryItem item = new SearchHistoryItem();
        item.name = "高德";
        item.datatype_spec = "0"; // 可以自己定义 ICON 标记
        // 2.2 搜索历史 POI
        SearchHistoryItem item1 = new SearchHistoryItem();
        item1.name = "肯德基";
        item1.category = "50301";
        item1.datatype_spec = "1"; // 可以自己定义 ICON 标记*/
        return mUserTrackAdapter.addSearchHistory(item);
    }

    /**
     * 删除搜索历史记录, 删除只需要赋值名字字段
     * @return
     */
    public int delSearchHistory(String name) {
        // name = "肯德基";
        return mUserTrackAdapter.delSearchHistory(name);
    }

    /**
     * 从sdk获取行程数据列表保存到本地
     */
    public void getDrivingRecordData() {
        mUserTrackAdapter.getDrivingRecordData();
    }

    /**
     * 获取行程数据列表（默认导航历史）
     * @return
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mUserTrackAdapter.getDrivingRecordDataList();
    }

    /**
     * 获取巡航历史-行程数据列表（巡航历史）
     * @return
     */
    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mUserTrackAdapter.getDrivingRecordCruiseDataList();
    }

    /**
     * 获取所有行程总里程
     * @return
     */
    public int getTotalDuration() {
        return mUserTrackAdapter.getTotalDuration();
    }

    /**
     * 获取所有行程总时长
     * @return
     */
    public int getTotalDistance() {
        return mUserTrackAdapter.getTotalDistance();
    }

    /**
     * 设置行程信息
     * 写到同步库里面，点击立即同步同步库进行同步，先进行数据同步，再进行轨迹同步
     * @param trailDriveDataId
     * @param data
     * @return
     */
    public int setBehaviorData(String trailDriveDataId, String data) {
        return mUserTrackAdapter.setBehaviorData(trailDriveDataId, data);
    }

    /**
     * 根据ID删除行程信息
     * @param id
     * @return
     */
    public int delBehaviorData(String id) {
        return mUserTrackAdapter.delBehaviorData(id);
    }

    /**
     * 获取置顶id文件路径，用户同步数据
     * @param id
     * @return
     */
    public String getFilePath(String id) {
        return mUserTrackAdapter.getFilePath(id);
    }

    public int startGpsTrack(String psSavePath, String psFileName, long un32MsecRate) {
        String filePath = ""; // GPS轨迹文件保存路径
        String fileName = ""; // GPS轨迹文件名 timestamp_navitype_devicecode
        long nTimerRate = 2000; // 定时器触发间隔时间 单位:毫秒，不要低于 500
        // 返回 0 调用成功
        return mUserTrackAdapter.startGpsTrack(psSavePath, psFileName, nTimerRate);
    }

    public int closeGpsTrack(String psSavePath, String psFileName) {
        return mUserTrackAdapter.closeGpsTrack(psSavePath, psFileName);
    }

    public int obtainGpsTrackDepInfo(String psSavePath, String psFileName) {
        return mUserTrackAdapter.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    @Override
    public void notify(int eventType, int exCode) {
        for (UserTrackCallBack observer : callBacks) {
            observer.notify(eventType, exCode);
        }
    }

    @Override
    public void onStartGpsTrack(int n32SuccessTag, String psSavePath, String psFileName) {
        // 可以调用 setBehaviorData 接口 保存未完成的行程数据(没有终点信息)
        for (UserTrackCallBack observer : callBacks) {
            observer.onStartGpsTrack(n32SuccessTag, psSavePath, psFileName);
        }
    }

    @Override
    public void onCloseGpsTrack(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {
        // 1.判断轨迹数据是否有效， 基础版只保存 导航里程超过 5KM 的行程数据
        // 2.获取 开始打点时保存的 未完成的行程数据
        // 3.补全终点信息
        // 4.调用 setBehaviorData 接口 保存完成的 GPS轨迹信息
        for (UserTrackCallBack observer : callBacks) {
            observer.onCloseGpsTrack(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    @Override
    public void onGpsTrackDepInfo(int n32SuccessTag, String psSavePath, String psFileName, GpsTrackDepthBean depInfo) {
        // 1.显示轨迹信息
        for (UserTrackCallBack observer : callBacks) {
            observer.onGpsTrackDepInfo(n32SuccessTag, psSavePath, psFileName, depInfo);
        }
    }

    public static UserTrackPackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final UserTrackPackage ep = new UserTrackPackage();
    }

}
