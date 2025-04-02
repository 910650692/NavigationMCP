package com.fy.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.fy.navi.hmi.databinding.DialogDeleteChargestationBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/28
 * Description: [删除补能充电站]
 */
public class ChargeStationDeletTipDialog extends BaseFullScreenDialog<DialogDeleteChargestationBinding> {
    private IBaseDialogClickListener mListener;

    protected ChargeStationDeletTipDialog(final Context context, final IBaseDialogClickListener listener) {
        super(context);
        mListener = listener;
    }

    @Override
    protected DialogDeleteChargestationBinding initLayout() {
        return DialogDeleteChargestationBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.dialogCommit.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mListener != null) {
                    mListener.onCommitClick();
                }
                dismiss();
            }
        });
        mViewBinding.dialogCancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                dismiss();
            }
        });
    }
}
