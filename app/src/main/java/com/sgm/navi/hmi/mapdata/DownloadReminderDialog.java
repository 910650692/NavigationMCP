package com.sgm.navi.hmi.mapdata;

import android.content.Context;
import android.view.LayoutInflater;

import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogDownloadReminderBinding;
import com.sgm.navi.ui.dialog.BaseDialog;

public class DownloadReminderDialog extends BaseDialog<DialogDownloadReminderBinding> {

    private final Context mContext;
    private OnDialogClickListener mListener;

    public DownloadReminderDialog(final Context context) {
        super(context);
        mContext = context;
    }

    @Override
    protected DialogDownloadReminderBinding initLayout() {
        return DialogDownloadReminderBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void initListener() {
        mViewBinding.dialogConfirm.setOnClickListener(v -> {
            dismiss();
            if (mListener != null) {
                mListener.onCommitClick();
            }
        });

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            dismiss();
            if (mListener != null) {
                mListener.onCancelClick();
            }
        });
    }

    /**
     * 设置弹框内容
     *
     * @param isDownloading 是否正在下载
     */
    public void setContent(final boolean isDownloading) {
        if (isDownloading) {
            mViewBinding.dialogTitle.setText(mContext.getString(R.string.offline_dialog_download_pause_title));
            mViewBinding.dialogConfirm.setText(mContext.getString(R.string.offline_dialog_download_pause));
        } else {
            mViewBinding.dialogTitle.setText(mContext.getString(R.string.offline_dialog_download_continue_title));
            mViewBinding.dialogConfirm.setText(mContext.getString(R.string.offline_dialog_download_continue));
        }
    }

    public void setOnDialogClickListener(final OnDialogClickListener listener) {
        this.mListener = listener;
    }

    public interface OnDialogClickListener {

        /**
         * 确认按钮
         */
        void onCommitClick();

        /**
         * 取消按钮
         */
        void onCancelClick();
    }
}
