package com.fy.navi.service.adapter.user.usertrack;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.fy.navi.service.define.user.usertrack.SearchHistoryItemBean;

import java.util.ArrayList;
import java.util.Objects;

/**
 * @Description
 * @Author fh
 * @date 2024/12/26
 */
public class UserTrackAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(UserTrackAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "UserTrackImpl";
    private IUserTrackApi mUserTrackApi;

    private UserTrackAdapter() {
        mUserTrackApi = (IUserTrackApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initUserTrackService() {
        mUserTrackApi.initUserTrackService();
    }

    public void registerCallBack(String key, UserTrackAdapterCallBack callBack) {
        mUserTrackApi.registerCallBack(key, callBack);
    }

    public void removeCallBack(String key) {
        mUserTrackApi.unRegisterCallback(key);
    }

    public void unInitUserTrackService() {
        mUserTrackApi.unInitUserTrackService();
    }

    public  ArrayList<SearchHistoryItemBean> getSearchHistory(){
        return mUserTrackApi.getSearchHistory();
    }

    public int addSearchHistory(SearchHistoryItemBean item){
        return mUserTrackApi.addSearchHistory(item);
    }

    public int delSearchHistory(String name){
        return mUserTrackApi.delSearchHistory(name);
    }

    public int clearSearchHistory(int mode){
        return mUserTrackApi.clearSearchHistory(mode);
    }

    public int setBehaviorData(String id, String data) {
        return mUserTrackApi.setBehaviorData(id,data);
    }

    public int delBehaviorData(String id) {
        return mUserTrackApi.delBehaviorData(id);
    }

    public void getDrivingRecordData() {
        mUserTrackApi.getDrivingRecordData();
    }

    public ArrayList<DrivingRecordDataBean> getDrivingRecordDataList() {
        return mUserTrackApi.getDrivingRecordDataList();
    }

    public ArrayList<DrivingRecordDataBean> getDrivingRecordCruiseDataList() {
        return mUserTrackApi.getDrivingRecordCruiseDataList();
    }

    public int getTotalDuration() {
        return mUserTrackApi.getTotalDuration();
    }

    public int getTotalDistance() {
        return mUserTrackApi.getTotalDistance();
    }

    public String getFilePath(String id) {
        return mUserTrackApi.getFilePath(id);
    }

    public int startGpsTrack(String psSavePath, String psFileName, long un32MsecRate) {
        return mUserTrackApi.startGpsTrack(psSavePath, psFileName, un32MsecRate);
    }

    public int closeGpsTrack(String psSavePath, String psFileName) {
        return mUserTrackApi.closeGpsTrack(psSavePath, psFileName);
    }

    public int obtainGpsTrackDepInfo(String psSavePath, String psFileName) {
        return mUserTrackApi.obtainGpsTrackDepInfo(psSavePath, psFileName);
    }

    public static UserTrackAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final UserTrackAdapter ra = new UserTrackAdapter();
    }
}
