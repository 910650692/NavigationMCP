package com.sgm.navi.hmi.drivingrecord.recorddetails;

import android.os.Bundle;
import android.view.Window;

import androidx.annotation.NonNull;

import com.android.utils.ResourceUtils;
import com.android.utils.StringUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentDrivingRecordDetailsBinding;
import com.sgm.navi.hmi.setting.SettingCheckDialog;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;


public class DrivingRecordDetailsFragment extends BaseFragment<FragmentDrivingRecordDetailsBinding, DrivingRecordDetailsViewModel> {
    private  DrivingRecordDataBean mDrivingRecordDataBean = new DrivingRecordDataBean();
    private SettingCheckDialog mDeleteDivingRecordDialog;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_driving_record_details;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        ThreadManager.getInstance().postUi(() -> {
            updateRecordDetailsView(mDrivingRecordDataBean);
        });

        initDeleteDialog();
    }

    @Override
    public void onInitData() {
        getBundleData();
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        Logger.d("DrivingRecordDetailsFragment", "onReStoreFragment");
        restoreFragment();
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        if(mDrivingRecordDataBean != null){
            mViewModel.setBean(mDrivingRecordDataBean);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
    }

    /**
     * 获取bundle数据
     */
    public void getBundleData(){
        final Bundle bundle = getArguments();
        if (bundle != null) {
            mDrivingRecordDataBean = bundle.getParcelable(AutoMapConstant.RecordDetailsBundleKey.BUNDLE_RECORD_DERAILS);
        }
    }

    /**
     * 恢复fragment
     */
    private void restoreFragment(){
        mDrivingRecordDataBean = mViewModel.getBean();
        if(mViewModel.getIsDeleteDivingRecordDialog()){
            mDeleteDivingRecordDialog.show();
        }
    }

    /**
     * 清除弹窗
     */
    private void clearDialog(){
        if(mDeleteDivingRecordDialog.isShowing()){
            mDeleteDivingRecordDialog.dismiss();
        }
        mDeleteDivingRecordDialog = null;
    }

    /**
     * 更新行程详情UI数据
     * @param bean 行程详情数据
     */
    private void updateRecordDetailsView(final DrivingRecordDataBean bean) {
        if (bean != null) {
            mBinding.detailsStart.setText(StringUtils.strExcessLengthOmitted(bean.getStartPoiName(), 10));   // 设置起点
            mBinding.detailsEnd.setText(StringUtils.strExcessLengthOmitted(bean.getEndPoiName(), 15)); // 设置终点
            mBinding.detailsTime.setText(TimeUtils.convertDateFormat(bean.getStartTime())); // 该行程完成时间

            if (bean.getRideRunType() == 1) { //导航
                mBinding.detailsLevel.setText("导航");
            } else if (bean.getRideRunType() == 0) { //巡航
                mBinding.detailsLevel.setText("巡航");
            }

            mBinding.detailsGuideCount.setText(TimeUtils.getInstance().getDistanceMsg(bean.getRunDistance()));// 该行程行驶距离
            mBinding.detailsGuideTime.setText(TimeUtils.formatSecondsToHHMMSS(bean.getTimeInterval())); // 驾驶时长
            mBinding.detailsGuideSpeed.setText(String.valueOf(bean.getAverageSpeed())); // 平均速度
            mBinding.detailsGuideFastSpeed.setText(String.valueOf(bean.getMaxSpeed())); // 最快速度
        }
    }

    /**
     * 删除单条记录
     */
    public void initDeleteDialog() {
        mDeleteDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete_message))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_item_delete))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setIsDeleteDrivingRecordDialog(false);
                        if (mDrivingRecordDataBean != null) {
                            if (AccountPackage.getInstance().isLogin()) {
                                mViewModel.delBehaviorData(mDrivingRecordDataBean.getId());
                            }
                            mViewModel.deleteValueByFileName(mDrivingRecordDataBean.getId(),
                                    mDrivingRecordDataBean.getRideRunType());
                            LayerPackage.getInstance().addLayerItemOfUserTrackDepth(MapType.MAIN_SCREEN_MAIN_MAP, null, false);
                            ThreadManager.getInstance().postUi(() -> {
                                closeFragment(true);
                                ToastUtils.Companion.getInstance().showCustomToastView(
                                        ResourceUtils.Companion.getInstance().getString(R.string.driving_record_delete_success));
                            });

                        }

                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setIsDeleteDrivingRecordDialog(false);
                    }
                }).build();
        clearBackground(mDeleteDivingRecordDialog.getWindow());
    }

    /**
     * 清除背景色
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    /**
     * 显示删除记录弹窗
     */
    public void showDialog() {
        mDeleteDivingRecordDialog.show();
    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }

}
