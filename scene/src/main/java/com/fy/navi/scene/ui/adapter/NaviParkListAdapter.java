
package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviParkListItemBinding;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

public class NaviParkListAdapter extends RecyclerView.Adapter<NaviParkListAdapter.ResultHolder> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final List<PoiInfoEntity> mList;
    private INaviParkItemClickListener onItemClickListener;
    private int mSelectIndex;

    public int getSelectIndex() {
        return mSelectIndex;
    }

    public void setOnItemClickListener(INaviParkItemClickListener listener) {
        onItemClickListener = listener;
    }

    public NaviParkListAdapter() {
        this.mList = new ArrayList<>();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void notifyList(List<PoiInfoEntity> list, int select) {
        Logger.d(TAG, "NaviAddViaAdapter notifyList " + list);
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mSelectIndex = select;
        mList.clear();
        mList.addAll(list);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SceneNaviParkListItemBinding itemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.scene_navi_park_list_item, parent, false);
        return new ResultHolder(itemBinding);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, @SuppressLint("RecyclerView") int position) {
        final PoiInfoEntity poiInfo = mList.get(position);
        final List<ParkingInfo> parkingInfos = poiInfo.getParkingInfoList();
        if (ConvertUtils.isEmpty(parkingInfos)) {
            return;
        }
        final ParkingInfo parkingInfo = parkingInfos.get(0);
        holder.itemBinding.tvNumber.setText(String.valueOf(position + 1));
        holder.itemBinding.tvTitle.setText(poiInfo.getName());
        holder.itemBinding.tvDistance.setText(poiInfo.getDistance());
        holder.itemBinding.sclListItem.setOnClickListener(v -> {
            if (onItemClickListener != null && mSelectIndex != position) {
                onItemClickListener.onItemClick(position);
                mSelectIndex = position;
                notifyDataSetChanged();
            }
        });
        holder.itemBinding.viewNaviNow.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onNaviClick(position, mList.get(position));
            }
        });

        holder.itemBinding.tvSpace.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
        holder.itemBinding.tvTotal.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
        holder.itemBinding.tvDesc.setVisibility(parkingInfo.getSpaceTotal() > 0 ? View.VISIBLE : View.GONE);
        holder.itemBinding.tvSpace.setText(String.valueOf(parkingInfo.getSpaceFree()));
        holder.itemBinding.tvTotal.setText("/" + parkingInfo.getSpaceTotal());

        final String desc = parkIsCrowed(poiInfo) ? holder.itemBinding.tvDesc.getContext().getString(R.string.tense)
                : holder.itemBinding.tvDesc.getContext().getString(R.string.chong_zu);
        holder.itemBinding.tvDesc.setText(desc);
        holder.itemBinding.tvEnd.setVisibility(poiInfo.getIsEndPoint() ? View.VISIBLE : View.GONE);
        holder.itemBinding.getRoot().setBackgroundResource(position == mSelectIndex ? R.color.common_item_select_color : R.color.transparent);
    }

    @Override
    public int getItemCount() {
        Logger.d(TAG, "NaviAddViaAdapter getItemCount " + mList.size());
        return mList.size();
    }

    public void notifyItemSelect(int index) {
        final int originSelectIndex = mSelectIndex;
        if (mSelectIndex != index) {
            mSelectIndex = index;
            notifyItemChanged(index);
            notifyItemChanged(originSelectIndex);
            if (!ConvertUtils.isNull(onItemClickListener)) {
                onItemClickListener.onItemClick(index);
            }
        }
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SceneNaviParkListItemBinding itemBinding;

        public ResultHolder(SceneNaviParkListItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.itemBinding = resultItemBinding;
            this.itemBinding.setHolder(this);
        }
    }

    /***
     * 判断停车场是否紧张
     * -停车位紧张：总车位数<=30个，剩余车位<30% ；总车位数>30个，剩余车位<10% 或 剩余车位少于10个。
     */
    private boolean parkIsCrowed(final PoiInfoEntity poiInfo) {
        if (ConvertUtils.isNull(poiInfo) || ConvertUtils.isEmpty(poiInfo.getParkingInfoList())) {
            return false;
        }
        final ParkingInfo parkingInfo = poiInfo.getParkingInfoList().get(0);
        final int totalSize = parkingInfo.getSpaceTotal();
        final int spaceSize = parkingInfo.getSpaceFree();
        Logger.i(TAG, "parkIsCrowed", "totalSize:" + totalSize, "spaceSize:" + spaceSize);
        if (totalSize <= 0) return false;
        return (totalSize <= 30 && spaceSize * 1f / totalSize < 0.3) || (totalSize > 30 && (spaceSize * 1f / totalSize < 0.1 || spaceSize < 10));
    }
}