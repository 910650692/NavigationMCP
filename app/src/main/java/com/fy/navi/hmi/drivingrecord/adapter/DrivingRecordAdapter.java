package com.fy.navi.hmi.drivingrecord.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.TimeUtils;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemDrivingRecordBinding;
import com.fy.navi.service.define.user.usertrack.DrivingRecordDataBean;

import java.util.ArrayList;

public class DrivingRecordAdapter extends RecyclerView.Adapter<DrivingRecordAdapter.Holder> {
    private ArrayList<DrivingRecordDataBean> dataList = new ArrayList();
    OnItemClickListener itemClickListener;

    public DrivingRecordAdapter() {
    }

    public void setDrivingRecordList(ArrayList<DrivingRecordDataBean> drivingRecordList) {
        this.dataList = drivingRecordList;
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemDrivingRecordBinding drivingRecordBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_driving_record, parent, false);
        return new Holder(drivingRecordBinding);
    }

    @Override
    public int getItemCount() {
        if (dataList == null) {
            return 0;
        }
        return dataList.size();
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.drivingRecordBinding.setModel(dataList.get(position));

        // 最后一条数据下不显示分割线
        if (position == dataList.size() - 1) {
            holder.drivingRecordBinding.viewLine.setVisibility(View.GONE);
        } else {
            holder.drivingRecordBinding.viewLine.setVisibility(View.VISIBLE);
        }

        // 设置起点
        holder.drivingRecordBinding.itemDrivingStartName.setText(dataList.get(position).getStartPoiName());
        // 设置终点
        holder.drivingRecordBinding.itemDrivingEndName.setText(dataList.get(position).getEndPoiName());
        // 该行程行驶距离
        holder.drivingRecordBinding.itemDrivingDistance.setText(TimeUtils.getInstance().getDistanceString(dataList.get(position).getRunDistance()));
        // 该行程完成时间
        holder.drivingRecordBinding.itemDrivingDate.setText(dataList.get(position).getEndTime());
        //删除删除item
        holder.drivingRecordBinding.itemDrivingDelete.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemDeleteClick(position);
            }
        });
        // 查看行程详情
        holder.drivingRecordBinding.itemView.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemClick(position);
            }
        });
    }

    public class Holder extends RecyclerView.ViewHolder {
        public ItemDrivingRecordBinding drivingRecordBinding;

        public Holder(ItemDrivingRecordBinding drivingRecordBinding) {
            super(drivingRecordBinding.getRoot());
            this.drivingRecordBinding = drivingRecordBinding;
            drivingRecordBinding.setHolder(this);
        }
    }

    public void removeItem(int index) {
        dataList.remove(index);
        notifyItemRemoved(index);
    }

    public interface OnItemClickListener {
        void onItemClick(int index);

        void onItemDeleteClick(int index);
    }
}