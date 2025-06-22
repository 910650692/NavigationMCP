package com.sgm.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ItemCityDataBinding;
import com.sgm.navi.hmi.utils.StringUtils;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;

public class CityMapDataAdapter extends RecyclerView.Adapter<CityMapDataAdapter.Holder> {
    private Context mContext;
    public ArrayList<CityDataInfo> mCityDataInfoList = new ArrayList<>();
    private OnItemClickListener mItemClickListener;

    public CityMapDataAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * 设置数据
     * @param list
     */
    public void setData(final ArrayList<CityDataInfo> list) {
        mCityDataInfoList = list;
        notifyDataSetChanged();
    }

    public int findPositionById(int adCode) {
        for (int i = 0; i < mCityDataInfoList.size(); i++) {
            if (mCityDataInfoList.get(i).getAdcode() == adCode) {
                return i;
            }
        }
        return RecyclerView.NO_POSITION; // -1
    }

    /**
     * 更新子项数据
     * @param childId
     * @param newValue
     */
    public void updateChild(int childId, CityDownLoadInfo newValue) {
        int childPosition = findPositionById(childId);
        for (CityDataInfo child : mCityDataInfoList) {
            if (child.getAdcode() == childId) {
                child.setDownLoadInfo(newValue);
                notifyItemChanged(childPosition);
                return;
            }
        }
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final ItemCityDataBinding cityDataBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_city_data, parent, false);
        return new Holder(cityDataBinding);
    }

    @Override
    public int getItemCount() {
        if (mCityDataInfoList == null) {
            return 0;
        }
        return mCityDataInfoList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mCityDataBinding.setModel(mCityDataInfoList.get(position));

        //城市下载状态
        final CityDownLoadInfo downloadItem = mCityDataInfoList.get(position).getDownLoadInfo();

        //非已下载状态，禁止侧滑删除
        holder.mCityDataBinding.swipeMenuLayout.setSwipeEnabled(downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS);

        //城市名称
        final String cityName = mCityDataInfoList.get(position).getName();
        holder.mCityDataBinding.itemCityName.setText(cityName);

        //城市数据包大小
        final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize().longValue());
        holder.mCityDataBinding.itemCityData.setText(sizeString); // sizeString

        // 下载按钮状态
        holder.mCityDataBinding.itemDownloadStatus.parseDownloadStatusInfo(downloadItem);

        final boolean isShowDownloadProgress = downloadItem.getTaskState()  == UserDataCode.TASK_STATUS_CODE_DOING
            || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE
            || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_UNZIPPING
            || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE;
        if (isShowDownloadProgress) {
            holder.mCityDataBinding.downloadProgress.setProgress((int) Math.floor(downloadItem.getPercent()));
            holder.mCityDataBinding.downloadProgress.setVisibility(View.VISIBLE);
        } else {
            holder.mCityDataBinding.downloadProgress.setVisibility(View.GONE);
        }

        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(mCityDataInfoList.get(position).getAdcode());

        //按钮操作
        holder.mCityDataBinding.itemDownloadStatus.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                        downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                        downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                    mItemClickListener.pauseAllTask(cityAdcodes);
                } else if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                        downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                        downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                        downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX) {
                    mItemClickListener.startAllTask(cityAdcodes);
                }
            }
        });

        holder.mCityDataBinding.itemDrivingDelete.setOnClickListener(v -> {
            holder.mCityDataBinding.swipeMenuLayout.smoothClose();
            if (mItemClickListener != null) {
                mItemClickListener.deleteAllTask(cityAdcodes);
            }
        });

    }

    public static class Holder extends RecyclerView.ViewHolder {
        private ItemCityDataBinding mCityDataBinding;

        public Holder(final ItemCityDataBinding cityDataBinding) {
            super(cityDataBinding.getRoot());
            this.mCityDataBinding = cityDataBinding;
            cityDataBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {

        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);

    }
}

