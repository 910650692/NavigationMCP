package com.sgm.navi.hmi.drivingrecord.recordsetting;

import android.view.Window;

import com.android.utils.ResourceUtils;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentRecordSettingBinding;
import com.sgm.navi.hmi.setting.SettingCheckDialog;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;

public class RecordSettingFragment extends BaseFragment<FragmentRecordSettingBinding, RecordSettingViewModel> {

    private SettingCheckDialog mClearDivingRecordDialog;
    @Override
    public int onLayoutId() {
        return R.layout.fragment_record_setting;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initDialog();
        mViewModel.initView();
    }

    @Override
    public void onInitData() {

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        restoreFragment();
    }

    /**
     * 恢复Fragment的状态
     */
    private void restoreFragment(){
        if(mViewModel.getIsClearDivingRecordDialog()){
            mClearDivingRecordDialog.show();
        }
    }

    /**
     * 清除Dialog
     */
    private void clearDialog(){
        if(mClearDivingRecordDialog.isShowing()){
            mClearDivingRecordDialog.dismiss();
        }
        mClearDivingRecordDialog = null;
    }

    /**
     * 初始化Dialog
     */
    public void initDialog() {

        mClearDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_content))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setIsClearDivingRecordDialog(false);
                        if (AccountPackage.getInstance().isLogin()) {
                            final ArrayList<DrivingRecordDataBean> drivingRecordDataBeans =
                                    mViewModel.getDrivingRecordDataFromSdk();
                            for (DrivingRecordDataBean drivingRecordDataBean : drivingRecordDataBeans) {
                                mViewModel.delBehaviorData(String.valueOf(drivingRecordDataBean.getId()));
                            }
                        }
                        mViewModel.deleteValueByKey(2);
                        setClearButtonEnable(false);
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setIsClearDivingRecordDialog(false);
                    }
                }).build();
        clearBackground(mClearDivingRecordDialog.getWindow());
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
     * 展示清除记录对话框
     */
    public void clearDivingRecord() {
        mClearDivingRecordDialog.show();
    }

    /**
     * 设置清除按钮可用性
     * @param enable 是否可用
     */
    public void setClearButtonEnable(final boolean enable) {
        mBinding.recordSettingClearBtn.setEnabled(enable);
        mBinding.recordSettingClearBtn.setAlpha(enable ? 1.0f : 0.5f);
        mBinding.recordSettingClearFocus.setEnabled(enable);
    }
}
