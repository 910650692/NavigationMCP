package com.sgm.navi.scene.ui.poi;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import androidx.databinding.ViewDataBinding;
import com.sgm.navi.scene.databinding.SceneDialogDeleteChargestationBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class ChargeStationDeletTipDialog extends BaseFullScreenDialog<SceneDialogDeleteChargestationBinding> {
    private IBaseDialogClickListener mListener;

    protected ChargeStationDeletTipDialog(final Context context, final IBaseDialogClickListener listener) {
        super(context);
        mListener = listener;
    }

    @Override
    protected SceneDialogDeleteChargestationBinding initLayout() {
        return SceneDialogDeleteChargestationBinding.inflate(LayoutInflater.from(getContext()));
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
