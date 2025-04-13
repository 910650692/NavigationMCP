package com.fy.navi.scene.ui.adapter;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ChargeEquipmentItemBinding;
import com.fy.navi.scene.ui.poi.ChargeStationConfirmDialog;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargePriceInfo;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;

public class ChargeEquipmentListAdapter extends RecyclerView.Adapter<ChargeEquipmentListAdapter.equipmentHolder> {
    private List<ChargeEquipmentInfo> equipmentInfoList = new ArrayList<>();
    private Context mContext;

    public ChargeEquipmentListAdapter(final Context context){
        this.mContext = context;
    }
    @NonNull
    @Override
    public ChargeEquipmentListAdapter.equipmentHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ChargeEquipmentItemBinding chargeEquipmentItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.charge_equipment_item,parent,false);
        return new equipmentHolder(chargeEquipmentItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ChargeEquipmentListAdapter.equipmentHolder holder, int position) {
        final ChargeEquipmentInfo info = equipmentInfoList.get(position);
        if(info == null){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChargeEquipmentInfo is null");
            return;
        }
        holder.mBinding.setEquipmentInfo(info);
        holder.mBinding.chargeReservationButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                openReservationDialog(info);
            }
        });
    }

    @Override
    public int getItemCount() {
        if (equipmentInfoList == null) {
            return 0;
        }
        return equipmentInfoList.size();
    }

    // 更新列表
    public void notifyList(List<ChargeEquipmentInfo> list){
        equipmentInfoList.addAll(list);
        notifyDataSetChanged();
    }

    // 打开预约确认弹窗
    public void openReservationDialog(ChargeEquipmentInfo info){
        new ChargeStationConfirmDialog.Build(mContext).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                // 预约行为

            }
        })
        .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_title))
        .setTip(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip))
        .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip_confirm))
        .build().show();
    }

    public static class equipmentHolder extends RecyclerView.ViewHolder{
        public ChargeEquipmentItemBinding mBinding;
        public equipmentHolder(@NonNull ChargeEquipmentItemBinding binding) {
            super(binding.getRoot());
            this.mBinding = binding;
        }
    }
}
