package com.fy.navi.hmi.drivingrecord.recordsetting;

import android.view.Window;

import com.android.utils.ResourceUtils;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentRecordSettingBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

public class RecordSettingFragment extends BaseFragment<FragmentRecordSettingBinding, RecordSettingViewModel> {

    private SettingCheckDialog clearDivingRecordDialog;
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

    public void initDialog() {

        clearDivingRecordDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_content))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.driving_record_setting_dialog_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {

                    }

                    @Override
                    public void onCancelClick() {

                    }
                }).build();
        clearBackground(clearDivingRecordDialog.getWindow());
    }

    private void clearBackground(Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    public void clearDivingRecord() {
        clearDivingRecordDialog.show();
    }
}
