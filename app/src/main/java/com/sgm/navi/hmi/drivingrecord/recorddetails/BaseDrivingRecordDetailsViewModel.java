package com.sgm.navi.hmi.drivingrecord.recorddetails;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;


public class BaseDrivingRecordDetailsViewModel extends BaseViewModel<DrivingRecordDetailsFragment, DrivingRecordDetailsModel> {

    private MutableLiveData<DrivingRecordDataBean> bean = new MutableLiveData<>();

    private MutableLiveData<Boolean> isDeleteDivingRecordDialog = new MutableLiveData<>(false);

    public BaseDrivingRecordDetailsViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected DrivingRecordDetailsModel initModel() {
        return new DrivingRecordDetailsModel();
    }

    //返回上一页
    public Action mDrivingRecordDetailsBack = () -> {
        LayerPackage.getInstance().addLayerItemOfUserTrackDepth(MapType.MAIN_SCREEN_MAIN_MAP, null, false);
        closeFragment(true);
    };

    //返回上一页
    public Action mDeleteRecord= () -> {
        mView.showDialog();
        setIsDeleteDrivingRecordDialog(true);
    };

    /**
     * 删除行程详情
     * @param id 行程详情id
     */
    public void delBehaviorData(final String id) {
        mModel.delBehaviorData(id);
    }

    /**
     * 通过数据type删除其对应info
     * @param id 数据id
     * @param runType 数据类型
     */
    public void deleteValueByFileName(final String id, final int runType) {
        mModel.deleteValueByFileName(id, runType);
    }

    /**
     * 设置bean
     * @param bean
     */
    public void setBean(DrivingRecordDataBean bean){
        this.bean.setValue(bean);
    }

    /**
     * 获取bean
     */
    public DrivingRecordDataBean getBean(){
        return bean.getValue();
    }

    /**
     * 获取是否显示删除记录dialog
     */
    public boolean getIsDeleteDivingRecordDialog() {
        return Boolean.TRUE.equals(isDeleteDivingRecordDialog.getValue());
    }

    /**
     * 设置是否显示删除记录dialog
     */
    public void setIsDeleteDrivingRecordDialog(Boolean isDeleteDrivingRecordDialog) {
        this.isDeleteDivingRecordDialog.setValue(isDeleteDrivingRecordDialog);
    }

    @Override
    protected void onBackPressed() {
        mView.showDialog();
        setIsDeleteDrivingRecordDialog(true);
    }
}
